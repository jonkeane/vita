/*
 This software is Copyright (c) 2010
 Michael Wild. All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:

 - Redistributions of source code must retain the above copyright
 notice, this list of conditions and the following disclaimer.

 - Redistributions in binary form must reproduce the above copyright
 notice, this list of conditions and the following disclaimer in
 the documentation and/or other materials provided with the
 distribution.

 - Neither the name of Michael Wild nor the names of any
 contributors may be used to endorse or promote products derived
 from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

// link with -framework Foundation -framework CoreServices

// Usage: convertBibDeskLinks <infile> <outfile>

#import <stdio.h>
#import <CoreServices/CoreServices.h>
#import <Foundation/Foundation.h>

// add a factory function to NSData that decodes an base64 encoded string
@interface NSData (InitWithBase64)
+(NSData*) dataWithBase64String:(NSString*)string;
@end

@implementation NSData (InitWithBase64)
+(NSData*) dataWithBase64String:(NSString*)string
{
  const char* cstring = [string cStringUsingEncoding:NSASCIIStringEncoding];
  NSMutableData* data = [NSMutableData dataWithCapacity:
                         (lround(strlen(cstring)*3./4.) + 1)];
  int nBits = 0;
  unsigned int outBuf = 0;
  unsigned char bits;
  unsigned char c;
  while ((c = *cstring++) != 0) {
    if (c >= 'A' && c <= 'Z')
      bits = c - 'A';
    else if (c >= 'a' && c <= 'z')
      bits = c - 'a' + 26;
    else if (isdigit(c))
      bits = c - '0' + 52;
    else if (c == '+')
      bits = 62;
    else if (c == '/')
      bits = 63;
    else if (c == '=')
      break;
    else
      continue;

    // push next 6 bits and increase bit count
    outBuf = (outBuf << 6) | bits;
    nBits += 6;
    if (nBits >= 8) {
      // pop byte and append to data
      nBits -= 8;
      unsigned char outByte = (outBuf >> nBits) & 0xff;
      [data appendBytes:&outByte length:1];
    }
  }

  return [NSData dataWithData:data];
}
@end

// here's the magic (a bit messy, I agree)
int main (int argc, const char* argv[]) {
  NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init];
  NSFileHandle* outFile = nil;
  int status = 0;
  // argument parsing
  if (argc != 3) {
    fprintf(stderr, "Usage: %s infile outfile\n", argv[0]);
    status = 1;
    goto FINALIZE;
  }
  // read input file contents
  NSString* inName = [NSString stringWithCString:argv[1]
                                        encoding:NSUTF8StringEncoding];
  NSError* err = nil;
  NSString* contents = [NSString stringWithContentsOfFile:inName
                                                 encoding:NSUTF8StringEncoding
                                                    error:&err];
  if (err) {
    NSLog(@"Error: Failed to read from input file: %@",
          [err localizedDescription]);
    status = 1;
    goto FINALIZE;
  }
  // open output file
  int outFd = open(argv[2], O_WRONLY|O_TRUNC|O_CREAT,
                   S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH);
  if (outFd < 0) {
    perror("Error: Failed opening output file");
    status = 1;
    goto FINALIZE;
  }
  outFile = [[NSFileHandle alloc] initWithFileDescriptor:outFd];

  // loop over lines
  for (NSString* line in [contents componentsSeparatedByString:@"\n"]) {
    NSMutableString* line_mod = [NSMutableString stringWithString:line];
    // find lines containing "Bdsk-File"
    NSRange range_key = [line_mod rangeOfString:@"Bdsk-File"];
    if (range_key.location != NSNotFound) {
      // find "{" and "}"
      NSUInteger pos_start =
        [[line_mod substringFromIndex:range_key.location]
          rangeOfString:@"{"].location;
      pos_start += range_key.location-1;
      NSUInteger pos_end =
        [[line_mod substringFromIndex:range_key.location]
          rangeOfString:@"}"].location;
      pos_end += range_key.location-1;
      // construct range between { and } and get base64 encoded data
      NSRange range_b64 = NSMakeRange(pos_start+2,
                                      pos_end-pos_start-1);
      NSString* pb64 = [line_mod substringWithRange:range_b64];
      // decode the data
      NSData* pbin = [NSData dataWithBase64String:pb64];
      // get the dictionary
      NSDictionary* plist = [NSKeyedUnarchiver unarchiveObjectWithData:pbin];
      // retrieve alias data
      NSData* aliasData = [plist objectForKey:@"aliasData"];
      NSUInteger len = [aliasData length];
      if (!len) {
        NSLog(@"Error: aliasData is empty");
        status = 1;
        goto FINALIZE;
      }
      // restore the AliasHandle
      Handle h = NewHandle(len);
      memmove((void*)*h, [aliasData bytes], len);
      // resolve the alias
      FSRef fsRef;
      Boolean wasChanged;
      OSErr err = FSResolveAlias(NULL, (AliasHandle)h, &fsRef, &wasChanged);
      DisposeHandle(h);
      if (noErr != err)
      {
        NSLog(@"Error: Failed to resolve alias");
        status = 1;
        goto FINALIZE;
      }
      // retrieve the path (are you kidding me???)
      CFURLRef resolvedUrl = CFURLCreateFromFSRef(kCFAllocatorDefault, &fsRef);
      if (!resolvedUrl)
      {
        NSLog(@"Error: Failed to resolve FSRef");
        status = 1;
        goto FINALIZE;
      }
      NSString* resolvedPath =
        (NSString*)CFURLCopyFileSystemPath(resolvedUrl, kCFURLPOSIXPathStyle);
      CFRelease(resolvedUrl);
      // construct new entry
      [line_mod replaceCharactersInRange:range_b64 withString:resolvedPath];
      [resolvedPath release];
      NSRange range_entry = NSMakeRange(range_key.location,
                                        pos_start-range_key.location);
      [line_mod replaceCharactersInRange:range_entry
                             withString:@"file ="];
    }
    // write new entry to output file
    [line_mod appendString:@"\n"];
    [outFile writeData:[line_mod dataUsingEncoding:NSUTF8StringEncoding]];
  }

FINALIZE:
  // clean up
  if (outFile) {
    [outFile closeFile];
    [outFile dealloc];
  }
  [pool drain];
  return status;
}
