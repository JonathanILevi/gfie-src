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

#include "imgformats.h"

#include <openjpeg.h>
#include <png.h>
#include <stdlib.h>

typedef struct
{
	IOFunction process;
	void *userData;
} IODelegate;

IODelegate *createIODelegate(IOFunction f, void *userData)
{
	IODelegate *d = NEW(IODelegate);
	if (d == NULL) return d;
	d->process = f;
	d->userData = userData;
	return d;
}

void freeIODelegate(IODelegate *d)
{
	free(d);
}

void pngReadWithIODelegate(png_structp png_ptr, png_bytep data, png_size_t length)
{
	IODelegate *d = (IODelegate*)png_get_io_ptr(png_ptr);
	if ((int64_t)d->process(d->userData, (char*)data, length) < (int64_t)length)
        png_error(png_ptr, "cannot read/write the specified number of bytes from/to stream");
}

#define pngWriteWithIODelegate pngReadWithIODelegate

void pngFlushWithIODelegate(png_structp png_ptr)
{
	// do nothing
}

// expands images to RGBA and handles interlacing
// returns number of passes needed (interlacing stuff)
int pngToRGBA(png_structp png_ptr, png_infop info_ptr, int bitDepth, int colorType, int interlaceType)
{
	int result = 0;

	if (colorType == PNG_COLOR_TYPE_PALETTE) png_set_palette_to_rgb(png_ptr);
    if (colorType == PNG_COLOR_TYPE_GRAY && bitDepth < 8) png_set_expand_gray_1_2_4_to_8(png_ptr);
    if (png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS)) png_set_tRNS_to_alpha(png_ptr);
    if (bitDepth == 16) png_set_strip_16(png_ptr);
    if (colorType == PNG_COLOR_TYPE_GRAY || colorType == PNG_COLOR_TYPE_GRAY_ALPHA)
        png_set_gray_to_rgb(png_ptr);

	png_read_update_info(png_ptr, info_ptr);
	colorType = png_get_color_type(png_ptr, info_ptr);
	if (colorType == PNG_COLOR_TYPE_RGB) png_set_add_alpha(png_ptr, 0xff, PNG_FILLER_AFTER);

	result = (interlaceType == PNG_INTERLACE_ADAM7 ? png_set_interlace_handling(png_ptr) : 1);

	png_read_update_info(png_ptr, info_ptr);
	return result;
}

EXPORT mybool pngLoad(Color32 *bm, int32_t *w, int32_t *h, double *dpi,
    IOFunction readCallback, void *userData)
{
	// Clean up and return
	#define LOAD_RET(result)	{\
		if (png_ptr) png_destroy_read_struct(&png_ptr, info_ptr ? &info_ptr : (png_infopp)NULL, (png_infopp)NULL); \
		if (stream) freeIODelegate(stream);\
		return (result);\
	}

	png_structp png_ptr = NULL;
	png_infop info_ptr = NULL;
	IODelegate *stream = NULL;
	int i, y;

	// init
//	mybool is_png = !png_sig_cmp(pngData, 0, min(8, pngDataSize));
//	if (!is_png) LOAD_RET(false);
	png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
	if (!png_ptr) LOAD_RET(false);
	info_ptr = png_create_info_struct(png_ptr);
    if (!info_ptr) LOAD_RET(false);

	// this is the exception 'catch' block :D
	if (setjmp(png_jmpbuf(png_ptr))) LOAD_RET(false);

	// create the stream
	stream = createIODelegate(readCallback, userData);
	if (stream == NULL) LOAD_RET(false);
	png_set_read_fn(png_ptr, stream, pngReadWithIODelegate);

	// read image info
	png_read_info(png_ptr, info_ptr);
	*w = png_get_image_width(png_ptr, info_ptr);
	*h = png_get_image_height(png_ptr, info_ptr);
	int colorType = png_get_color_type(png_ptr, info_ptr);
	int bitDepth = png_get_bit_depth(png_ptr, info_ptr);
	int interlaceType = png_get_interlace_type(png_ptr, info_ptr);
	// if bm is null, then we do not need to read the whole image
	if (bm == NULL) LOAD_RET(true);

    // load dpi data
    if (dpi)
    {
        *dpi = 0;
        uint32_t res = png_get_pixels_per_meter(png_ptr, info_ptr);
        if (res != 0)
            *dpi = res * 0.0254;
    }

	// transform the image first and handle interlacing
	int number_of_passes = pngToRGBA(png_ptr, info_ptr, bitDepth, colorType, interlaceType);
	// now we have an RGBA png

	// read image data
	for (i = 0; i < number_of_passes; i++)
	{
		Color32 *rowPointer = bm;
		for (y = 0; y < *h; y++)
		{
			png_read_row(png_ptr, (png_bytep)rowPointer, NULL);
			rowPointer += *w;
		}
	}

	// end reading PNG
	png_read_end(png_ptr, NULL);

	// remove inverted color
	Color32 *p = bm;
	for (i = 0; i < *w * *h; i++)
	{
		if ((*p & cl32Opaque) == 0) *p = cl32Transparent;
		p++;
	}

	LOAD_RET(true);

	#undef LOAD_RET
}

EXPORT mybool pngSave(Color32 *bm, int32_t w, int32_t h, double dpi,
    IOFunction writeCallback, void *userData, int32_t compressionLevel)
{
	// Clean up and return
	#define SAVE_RET(result)	{\
		if (png_ptr) png_destroy_write_struct(&png_ptr, info_ptr ? &info_ptr : (png_infopp)NULL); \
		if (stream) freeIODelegate(stream);\
		return (result);\
	}

	png_structp png_ptr = NULL;
	png_infop info_ptr = NULL;
	IODelegate *stream = NULL;
	int y;

	// init
	png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
	if (!png_ptr) SAVE_RET(false);
	info_ptr = png_create_info_struct(png_ptr);
    if (!info_ptr) SAVE_RET(false);

	// this is the exception 'catch' block :D
	if (setjmp(png_jmpbuf(png_ptr))) SAVE_RET(false);

	// create the stream
	stream = createIODelegate(writeCallback, userData);
	if (stream == NULL) SAVE_RET(false);
	png_set_write_fn(png_ptr, stream, pngWriteWithIODelegate, pngFlushWithIODelegate);

	// set compression level
	png_set_compression_level(png_ptr, compressionLevel);
	// set image header
	png_set_IHDR(png_ptr, info_ptr, w, h, 8, PNG_COLOR_TYPE_RGB_ALPHA,
		PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);
    if (dpi != 0.0)
    {
        uint32_t res = (uint32_t)(dpi / 0.0254 + 0.5);
        png_set_pHYs(png_ptr, info_ptr, res, res, 1);
    }
	png_write_info(png_ptr, info_ptr);

	// write image data
	Color32 *rowPointer = bm;
	for (y = 0; y < h; y++)
	{
		// no need to deal with inverted color
		// it will appear as transparent in output PNG
		png_write_row(png_ptr, (png_bytep)rowPointer);
		rowPointer += w;
	}

	// end writing PNG
	png_write_end(png_ptr, NULL);

	SAVE_RET(true);

	#undef SAVE_RET
}

/////////////////// JPEG-2000

void opj_error_callback(const char *msg, void *client_data) {}
void opj_warning_callback(const char *msg, void *client_data) {}
void opj_info_callback(const char *msg, void *client_data) {}

EXPORT mybool jp2Load(Color32 **bm, int32_t *w, int32_t *h, void *data, int dataSize)
{
	// Clean up and return
	#define LOAD_RET(result)	{\
		if (!(result) && *bm) {free(*bm); *bm = NULL;} \
		if (image) opj_image_destroy(image);\
		if (cio) opj_cio_close(cio);\
		if (dinfo) opj_destroy_decompress(dinfo);\
		return (result);\
	}

	int i, j, c;
	opj_event_mgr_t    event_mgr;
	opj_dparameters_t  parameters;
	opj_dinfo_t        *dinfo = NULL;
	opj_cio_t          *cio = NULL;
	opj_image_t        *image = NULL;

    // validate parameters
    if (!bm || !w || !h || !data || dataSize <= 0) LOAD_RET(false);

    *bm = NULL;
    *w = 0;
    *h = 0;

	memset(&event_mgr, 0, sizeof(opj_event_mgr_t));
	event_mgr.error_handler = opj_error_callback;
	event_mgr.warning_handler = opj_warning_callback;
	event_mgr.info_handler = opj_info_callback;

	opj_set_default_decoder_parameters(&parameters);

	dinfo = opj_create_decompress(CODEC_JP2);
	if (!dinfo) LOAD_RET(false);
	opj_set_event_mgr((opj_common_ptr)dinfo, &event_mgr, NULL);
	opj_setup_decoder(dinfo, &parameters);

	cio = opj_cio_open((opj_common_ptr)dinfo, (unsigned char*)data, dataSize);
	if (!cio) LOAD_RET(false);

	image = opj_decode(dinfo, cio);
	if (!image) LOAD_RET(false);

    // TODO: convert YUV pixels to RGB
    if (image->color_space == CLRSPC_SYCC) LOAD_RET(false);

    // we don't need these objects any more
	opj_cio_close(cio); cio = NULL;
	opj_destroy_decompress(dinfo); dinfo = NULL;

	*w = image->comps[0].w;
	*h = image->comps[0].h;
    int imageSize = 4 * *w * *h;
	*bm = (Color32*)malloc(imageSize);
	if (!*bm) LOAD_RET(false);
	memset(*bm, 0, imageSize);

	int adjust[4];
	for(c = 0; c < image->numcomps; c++)
	{
		int depth = image->comps[c].prec;
		if(depth > 8) adjust[c] = depth - 8; else adjust[c] = 0;
	}

	for (i = 0; i < *h; i++)
	{
		for(j = 0; j < *w; j++)
		{
            int srcIndex = i * *w+j;
            int srcPixel[4];

            // get source pixel
            for (c = 0; c < image->numcomps; c++)
            {
                int v = image->comps[c].data[srcIndex];
                v += (image->comps[c].sgnd ? 1 << (image->comps[c].prec - 1) : 0);
                srcPixel[c] = (uint8_t) ((v >> adjust[0])+((v >> (adjust[0]-1))%2));
            }

            // set destination pixel
			Color32 *pDest = &(*bm)[i * *w + j];
			switch (image->numcomps)
			{
			    // Grayscale
			    case 1: *pDest = ((srcPixel[0]*0x010101) | cl32Opaque); break;
			    // Grayscale with Alpha
			    case 2: *pDest = ((srcPixel[0]*0x010101) | (srcPixel[1] << 24)); break;
			    // sRGB/sYCC
			    case 3: *pDest = (srcPixel[0] | (srcPixel[1] << 8) | (srcPixel[2] << 16) | cl32Opaque); break;
			    // sRGB with Alpha
			    case 4: *pDest = (srcPixel[0] | (srcPixel[1] << 8) | (srcPixel[2] << 16) | (srcPixel[3] << 24)); break;
			    // other values -> error
			    default: LOAD_RET(false);
			}

			// prevent inverted color from appearing
			if (((uint8_t*)pDest)[3] == 0) *pDest = cl32Transparent;
		} // for j
	} // for i

    // success
    LOAD_RET(true);

	#undef LOAD_RET
}

EXPORT mybool jp2Save(Color32 *bm, int32_t w, int32_t h, float quality, void **data, int *dataSize)
{
    // Clean up and return
	#define SAVE_RET(result)	{\
		if (!(result) && *data) {free(*data); *data = NULL;} \
		if (image) opj_image_destroy(image);\
        if (cio) opj_cio_close(cio);\
		if (cinfo) opj_destroy_compress(cinfo);\
		return (result);\
	}

	int                  i, j;
	opj_event_mgr_t      event_mgr;
	opj_cparameters_t    parameters;
	opj_image_cmptparm_t cmptparm[4];
	opj_image_t	     *image = NULL;
	opj_cinfo_t          *cinfo = NULL;
	opj_cio_t            *cio = NULL;

    if (!bm || !data || !dataSize) SAVE_RET(false);

	*data = NULL;
	*dataSize = 0;

	memset(&event_mgr, 0, sizeof(opj_event_mgr_t));
	event_mgr.error_handler = opj_error_callback;
	event_mgr.warning_handler = opj_warning_callback;
	event_mgr.info_handler = opj_info_callback;

	opj_set_default_encoder_parameters(&parameters);

    parameters.irreversible = (quality != 0);
    parameters.tcp_distoratio[0] = quality;
    parameters.tcp_numlayers = 1;
    parameters.cp_fixed_quality = 1;
/*	parameters.tcp_numlayers = 1;
	parameters.tcp_rates[0] = 1;
	parameters.cp_disto_alloc = 1;
	parameters.irreversible = 1;*/

	memset(cmptparm, 0, 4 * sizeof(opj_image_cmptparm_t));
	for(i = 0; i < 4; i++)
	{
		cmptparm[i].w = w;
		cmptparm[i].h = h;
		cmptparm[i].dx = 1;
		cmptparm[i].dy = 1;
		cmptparm[i].prec = 8;
		cmptparm[i].bpp = 8;
		cmptparm[i].sgnd = 0;
	}

	image = opj_image_create(4, cmptparm, CLRSPC_SRGB);
	if (!image) SAVE_RET(false);

	image->x0 = 0;
	image->y0 = 0;
	image->x1 = w;
	image->y1 = h;

	uint8_t *srcValue = (uint8_t*)bm;
	for (i = 0; i < h; i++) {
		for(j = 0; j < w; j++) {
			int p = i * w + j;

			image->comps[0].data[p] = *srcValue++; // r
			image->comps[1].data[p] = *srcValue++; // g
			image->comps[2].data[p] = *srcValue++; // b
			image->comps[3].data[p] = *srcValue++; // a
		}
	}

	cinfo = opj_create_compress(CODEC_JP2);
	if (!cinfo) SAVE_RET(false);
	opj_set_event_mgr((opj_common_ptr)cinfo, &event_mgr, NULL);
	opj_setup_encoder(cinfo, &parameters, image);

	cio = opj_cio_open((opj_common_ptr)cinfo, NULL, 0);
	if (!cio) SAVE_RET(false);

	int success = opj_encode(cinfo, cio, image, NULL);
	if (!success) SAVE_RET(false);

	*dataSize = cio_tell(cio);
	*data = malloc(*dataSize);
    if (!*data) SAVE_RET(false);

	memcpy(*data, cio->buffer, *dataSize);

    // success
    SAVE_RET(true);

    #undef SAVE_RET
}

EXPORT void jp2Free(void *p)
{
    if (p == NULL) return;
    free(p);
}
