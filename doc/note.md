
# R17.5 does not support the following features.

## map pattern matching with bounded variable.

```erlang
Erlang/OTP 18 [erts-7.0] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]
Eshell V7.0  (abort with ^G)
1> M = #{a=>1}.
#{a => 1}
2> A = a.
a
4> #{ A := X } = M.
#{a => 1}
5> B = b.
b
6> M#{B=>2}.
#{a => 1,b => 2}
7>
```



```
Erlang/OTP 17 [erts-6.4] [source] [64-bit] [smp:4:4] [async-threads:10] [kernel-poll:false]
Eshell V6.4  (abort with ^G)
1> M = #{a=>1}.
#{a => 1}
2> A = A.
* 1: variable 'A' is unbound
3> A = a.
a
4> #{ A := X } = M.
* 1: illegal use of variable 'A' in map
5> B = b.
b
6> M#{ B => 1}.
* 1: illegal use of variable 'B' in map
```


## `gen_server:stop`

```
(9022@timo-ThinkPad-E450c)41> m(gen_server).
Erlang/OTP 17 [erts-6.4] [source] [64-bit] [smp:4:4] [async-threads:10] [kernel-poll:false]
Eshell V6.4  (abort with ^G)
Module: gen_server
Compiled: March 31 2015, 12:46
Object file: /usr/local/lib/erlang/lib/stdlib-2.4/ebin/gen_server.beam
Compiler options:  [{outdir,"/net/isildur/ldisk/daily_build/17_prebuild_opu_o.2015-03-31_14/otp_src_17/lib/stdlib/src/../ebin"},
                    {i,"/net/isildur/ldisk/daily_build/17_prebuild_opu_o.2015-03-31_14/otp_src_17/lib/stdlib/src/../include"},
                    {i,"/net/isildur/ldisk/daily_build/17_prebuild_opu_o.2015-03-31_14/otp_src_17/lib/stdlib/src/../../kernel/include"},
                    warnings_as_errors,debug_info]
Exports:
abcast/2                      multi_call/2
abcast/3                      multi_call/3
behaviour_info/1              multi_call/4
call/2                        reply/2
call/3                        start/3
cast/2                        start/4
enter_loop/3                  start_link/3
enter_loop/5                  start_link/4
enter_loop/4                  system_code_change/4
format_status/2               system_continue/3
init_it/6                     system_get_state/1
module_info/0                 system_replace_state/2
module_info/1                 system_terminate/4
                              wake_hib/5
ok
```


but for R18

```
7> m(gen_server).
Module: gen_server
MD5: 4e0c8aca6db126294b3ea018529aa08e
Compiled: June 23 2015, 19:08
Object file: /usr/local/r18/lib/erlang/lib/stdlib-2.5/ebin/gen_server.beam
Compiler options:  [{outdir,"/net/isildur/ldisk/daily_build/18_prebuild_master-opu_o.2015-06-23_20/otp_src_18/lib/stdlib/src/../ebin"},
                    {i,"/net/isildur/ldisk/daily_build/18_prebuild_master-opu_o.2015-06-23_20/otp_src_18/lib/stdlib/src/../include"},
                    {i,"/net/isildur/ldisk/daily_build/18_prebuild_master-opu_o.2015-06-23_20/otp_src_18/lib/stdlib/src/../../kernel/include"},
                    warnings_as_errors,debug_info]
Exports:
abcast/2                      multi_call/3
abcast/3                      multi_call/4
behaviour_info/1              reply/2
call/2                        start/3
call/3                        start/4
cast/2                        start_link/3
enter_loop/3                  start_link/4
enter_loop/5                  stop/1
enter_loop/4                  stop/3
format_status/2               system_code_change/4
init_it/6                     system_continue/3
module_info/0                 system_get_state/1
module_info/1                 system_replace_state/2
multi_call/2                  system_terminate/4
                              wake_hib/5
ok
8>
```
