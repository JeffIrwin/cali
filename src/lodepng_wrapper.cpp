
#include <iostream>

#include <lodepng.h>

//extern "C" int write_png_(uint8_t* b, int& nx, int& ny, char* cf)
//extern "C" int write_png_(const char* filename, std::vector<unsigned char>& image, int width, int height)
extern "C" int write_png_(const char* filename, int32_t* image, int& width, int& height)
//extern "C" int write_png_(const char* filename, int8_t* image, int& width, int& height)
{
	std::cout << "starting write_png_() ...\n";
	std::cout << "filename  = " << filename << std::endl;
	std::cout << "width     = " << width    << std::endl;
	std::cout << "height    = " << height   << std::endl;

	unsigned uwidth  = width;
	unsigned uheight = height;
	
	//unsigned error = lodepng::encode(filename, image, uwidth, uheight);
	unsigned error = lodepng_encode32_file(filename, (const unsigned char*) image, uwidth, uheight);

	return error;
}

