### Desciprtion

An Apple //e native client for <http://asciiexpress.net/gameserver/>.  Fully describing this will take too long--just watch the video: <http://asciiexpress.net/gameserver/gameserverclient.mp4>.


### Download

```
git clone https://github.com/datajerk/gameserverclient.git
```

*or*

Download <https://github.com/datajerk/gameserverclient/archive/master.zip> and extract.


### Build Notes

#### Prerequisites

- `cl65` (<http://cc65.github.io/cc65/>)
- `c2d` (<https://github.com/datajerk/c2d>)
- `libqrencode` (<https://github.com/fukuchi/libqrencode>)
- `perl`
- `gcc`
- `figlet`


#### Build
```
make
```


### Test

#### Prerequisites

- MacOS
- Virtual ][ 9.2
- `curl`
- `zbarimg` (Google for `zbar-0.10.tar.bz2`)
- `tifftopnm` and `pnmtojpeg` from Netpbm (<http://netpbm.sourceforge.net/>)
- `sox` and `soxi` from SoX (<http://sox.sourceforge.net/>)

```
make test
```

#### Ad Hoc Testing

```
./quick.sh [search string or "random"]
./demo.sh [search string or "random"]
```

Example Ad Hoc Session:

```
$ ./demo.sh alien
1       Alien Ambush
2       Alien Downpour
3       Alien Game
4       Alien Munchies
5       Alien Typhoon

pick one: 2

Alien Downpour...LAUNCHED
```

Ad hoc example session video output: <http://asciiexpress.net/files/gameserverclienttest.mp4>


### Appendix

#### Zbar MacOS Build
```
sudo port install imagemagick qrencode
tar zxvf zbar-0.10.tar.bz2
cd zbar-0.10
./configure --disable-video --without-python --without-gtk --without-qt --with-libiconv-prefix=/opt/local
make -j
sudo make install
```

