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
- `curl`


#### Build
```
make
```


### Test

#### Prerequisites

- MacOS
- Virtual ][
- `zxing-cpp` (zxing command) for testing (<https://github.com/glassechidna/zxing-cpp>)
- `tifftopnm` and `pnmtojpeg` from Netpbm (<http://netpbm.sourceforge.net/>)
- `sox` and `soxi` from SoX (<http://sox.sourceforge.net/>)

```
make test
./quick.sh [search string or "random"]
./demo.sh [search string or "random"]
```


### Appendix

#### zxing-cpp build
```
git clone https://github.com/glassechidna/zxing-cpp
cd xzing-cpp
cd build
cmake -G "Unix Makefiles" ..
make
sudo cp zxing /usr/local/bin/
```

