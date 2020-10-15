/*
    Greenfish Icon Editor Pro
    Copyright (c) 2012-13 B. Szalkai

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef __IMGFORMATS_H
#define __IMGFORMATS_H

#include <stdint.h>

#ifndef false
#define false 0
#endif
#ifndef true
#define true 1
#endif

#define NEWA(x, count) ((x*)calloc(sizeof(x), count))
#define NEW(x) NEWA(x, 1)

#ifdef WINDOWS
#define CALLCONV _cdecl
#else
#define CALLCONV __attribute__((__cdecl__))
#endif

#if defined(_MSC_VER)
    //  Microsoft
    #define EXPORT extern "C" __declspec(dllexport) CALLCONV
#elif defined(__GNUC__)
    //  GCC
    #define EXPORT extern "C" __attribute__((visibility("default"))) CALLCONV
#else
    #define EXPORT
    #pragma warning Unknown dynamic link import/export semantics.
#endif

typedef char mybool;
typedef uint32_t Color32;
typedef int32_t Color;
#define cl32Transparent 0
#define cl32Opaque 0xff000000

// Callback routine to read/write 'count' bytes to/from 'data' from/to a stream resource specified by 'userData'
typedef int32_t CALLCONV (*IOFunction)(void *userData, char *data, int32_t count);

/////////////// PNG

// Uncompresses PNG data into 32-bit bitmap. Returns true if succeeded, false otherwise.
// w and h will contain image dimensions
// dpi will contain image dpi, or 0 if not set
// If bm==NULL, then it just returns image dimensions into w and h
EXPORT mybool pngLoad(Color32 *bm, int32_t *w, int32_t *h, double *dpi,
    IOFunction readCallback, void *userData);
// Saves bitmap as PNG image to stream.
EXPORT mybool pngSave(Color32 *bm, int32_t w, int32_t h, double dpi,
    IOFunction writeCallback, void *userData, int32_t compressionLevel);

//////////////// JPEG-2000

// Uncompresses JP2 data into 32-bit bitmap. Returns true if succeeded, false otherwise.
// w and h will contain image dimensions
// bm will contain a pointer to the image -- this must be freed with jp2Free when done with processing
// loads the image from (data, dataSize)
EXPORT mybool jp2Load(Color32 **bm, int32_t *w, int32_t *h, void *data, int32_t dataSize);
// Saves bitmap as JP2 image to memory
// data must be freed with jp2Free afterwards
// quality is the Peak Signal to Noise Ratio. if quality = 0, then lossless compression is used
EXPORT mybool jp2Save(Color32 *bm, int32_t w, int32_t h, float quality, void **data, int32_t *dataSize);
// Frees memory allocated by jp2Load and jp2Save
// If those functions return false, there is no need to call jp2Free, because the returned pointer is null
EXPORT void jp2Free(void *p);

#endif
