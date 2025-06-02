These are magic numbers for various operating systems, taken from Cosmopolitan Libc. 
The sysv ones say (ISC License. Copyright 2020 Justine Alexandra Roberts Tunney). 
I don't understand why she owns the sysv numbers but not the nt numbers. 

- https://github.com/jart/cosmopolitan/tree/f1e83d52403060d674161944e849b51f95707c9a
- https://justine.lol/ape.html

## sysv/consts.csv (libc/sysv/consts.sh)

- group, name, GNU_Systemd, GNU_Systemd_Aarch64, XNU, MacOS_Arm64, FreeBSD, OpenBSD, NetBSD

```
function syscon {
    echo "$1,$2,$3,$4,$5,$6,$7,$8,$9,${10}";
}
```

## sysv/syscalls.csv (libc/sysv/syscalls.sh)

```
function scall {
    echo "$1,$2,$3";
}

The Fifth Bell System Interface, Community Edition     ┌─────────────────────────┐
» so many numbers                                      │ legend                  │
                                                       ├─────────────────────────┤
                             GNU/Systemd┐              │   fff  │ unavailable    │
                                 2.6.18+│              │   800  │ cancellable    │
                             Mac OS X┐  │              │   sys_ │ wrapped        │
                                15.6+│  │              │ __sys_ │ wrapped twice  │
                          FreeBSD┐   │  │              └─────────────────────────┘
                              12+│   │  │
                       OpenBSD┐  │ ┌─│──│── XnuClass{1:Mach,2:Unix}
                          6.4+│  │ │ │  │
                     NetBSD┐  │  │ │ │  │
                       9.1+│  │  │ │ │  │
                          ┌┴┐┌┴┐┌┴┐│┬┴┐┌┴┐      Arm64
                        0x0000000000000000      0x000
```

## sysv/dos2errno.csv (libc/sysv/dos2errno.sh)

```
function dos {
    echo "$1,$2";
}
```

## sysv/errfuns

it's just errno/junkerr from sysv_consts (to_lower, to_upper). 

## nt/master.csv (libc/nt/master.sh)

- name, actual, dll, arity

```
function imp {
    echo "$1,$2,$3,$4";
}
```

## nt/errors.csv (libc/nt/errors.h)

- eval_and_dump_defined_numbers
- TODO: most of these are never used except for being renamed in windowsesque.h
