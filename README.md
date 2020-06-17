# play-with-turnstile

## exn-with-fcontrol.rkt
### TODO fix try-handle
* uncaught-exnの計算が汚い
* uncaught-exnの計算が不正確。e-exn ...には同じ名前のexnが複数個登場しうる。removeでは最初の一つしか除けない
* uncaought-exn = {e-exn ...} \ {s}, try-handleのexn = uncaught-exn ∪ handler-exnが正確な気がする
