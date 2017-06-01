#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>
#include <string.h>
#include <qrencode.h>

static int casesensitive = 1;
static int version = 0;
static QRecLevel level = QR_ECLEVEL_L;
static QRencodeMode hint = QR_MODE_8;

#define MAX_DATA_SIZE (7090 * 16) /* from the specification */

static int writetext(QRcode *qrcode)
{
	unsigned char *p, bytes[1000];
	int x, y, bit = 0, count=0;

	printf("width: %d\n",qrcode->width);

	p = qrcode->data;
	for(y=0; y<qrcode->width; y++) {
		for(x=0; x<qrcode->width; x++) {
			if(bit == 0) {
				bytes[count++] = 0;
				bit = 8;
			}

			bytes[count-1] *= 2;
			bytes[count-1] += (*p & 1);
			bit--;

			p++;
		}
	}

	for(x=0;x<bit;x++) {
		bytes[count-1] *= 2;
	}

	printf("bytes: ");
	for(x=0;x<count;x++) {
		printf("0x%02X ",bytes[x]);
	}
	printf("\n");

	return 0;
}

static QRcode *encode(const unsigned char *intext, int length)
{
	QRcode *code;

	code = QRcode_encodeString((char *)intext, version, level, hint, casesensitive);

	return code;
}

static void qrencode(const unsigned char *intext, int length)
{
	QRcode *qrcode;
	
	qrcode = encode(intext, length);
	if(qrcode == NULL) {
		perror("Failed to encode the input data");
		exit(EXIT_FAILURE);
	}
	writetext(qrcode);
	QRcode_free(qrcode);
}

int main(int argc, char **argv)
{
	unsigned char *intext = NULL;
	int length = 0;

	intext = (unsigned char *)argv[optind];
	length = strlen((char *)intext);

	qrencode(intext, length);

	return 0;
}
