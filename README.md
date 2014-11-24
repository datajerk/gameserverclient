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


#### Compile
```
cl65 -t none --listing --list-bytes 100 gameserverclient.s
```

#### Create Diskette Image
```
c2d gameserverclient,800 gameserverclient.dsk
```

Output:

```
Reading gameserverclient, type BINARY, start: $0800, length: 32664

Number of sectors:    128
Sector page range:    $08 - $87
After boot, jump to:  $0800

Writing gameserverclient to T:01/S:00 - T:08/S:15 on gameserverclient.dsk
```

