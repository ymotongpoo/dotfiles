TRR19 for Win32 の説明書

『これは何?』

TRR19 version 1.0 beta5を基にMule for Win32向けに移植したものです。
TRR自身の説明は、README.eucやinfoに譲ります。
README.eucに従った方法でインストールすれば、UNIX上でも動作します。



『動作環境』

実行には、当然、「Mule for Win32」が必要です。
Intel環境であれば、わずかな設定で動作可能と思います。
Intel環境以外では、trr_format.exeとtrr_update.exeを再コンパイルする必要
があります。

1997/3/18現在、動作確認のとれている環境は、以下の通りです。
  ・Windows95とW113の組み合わせ
  ・WindowsNT4.0 Workstation(Intel環境)とW113の組み合わせ



『ファイルを展開する』

このファイルを読んでいるならば、すでに展開は済んでいることでしょう。
WinZipで展開した場合、「TAR File Smart CR/LF Conversion」オプションがオ
フになっていることを確認したほうがいいかもしれません。(どうなんだろう?)

展開すると、trr19というディレクトリが作成され、その中にいくつかのファイ
ル(とサブディレクトリ)ができます。

以降における例は、以下の環境を想定します。
  ・環境変数HOMEの値は、c:\home\fooに設定されている。
  ・c:\home\fooに展開し、c:\home\foo\trr19が作成された。



『環境変数を設定する』

TRRDIRとTRRBINDIRの、2つの環境変数の設定が必要です。

  TRRDIR:
    CONTENTSファイル, textサブディレクトリおよびrecordサブディレクトリを
    含むディレクトリを、フルパスで指定します。
    この3つをセットで、別の場所に置くことも出来ます。
    ディレクトリ区切りをスラッシュ('/')で書いても大丈夫です…多分。
    例での場合は「c:\home\foo\trr19」です。

  TRRBINDIR:
    実行ファイル(trr_format.exeとtrr_update.exe)を含むディレクトリを、
    フルパスで指定します。
    ディレクトリ区切りをスラッシュ('/')で書いても大丈夫です。
    例での場合だと、こちらも「c:\home\foo\trr19」です。
    実行ファイルだけを別の場所に置くことも出来ます。

.emacsの中で指定する場合、以下のように設定します。
  (setenv "TRRDIR" "c:/home/foo/trr19"))
  (setenv "TRRBINDIR" "c:/home/foo/trr19"))

以下のようにも書くことができます。
  (setenv "TRRDIR" (expand-file-name "~/trr19"))
  (setenv "TRRBINDIR" (expand-file-name "~/trr19"))



『Muleにtrrのありかを教える』

.emacsに(autoload ...)を書きます。
例での場合だと、以下のようになります。
  (autoload 'trr "c:/home/foo/trr19/trr" nil t)

以下のようにも書くことができます。
  (autoload 'trr (expand-file-name "~/trr19/trr") nil t)



『起動の前に』

必ずREADME.eucやinfoに目を通してください。
これらは、特に変更を加えていません。
蛇足ながら、infoは、「C-u M-x info」して、trr.infoを指定すれば読めます。

その上でtrr.elに変更が必要な場合は、変更を加えてください。
(個人的には、TRR:installatorを変更してほしいです。 ^^;)

バイト・コンパイルを行ってもかまいません。

環境変数をAUTOEXEC.BATで指定したならば、Windowsを再起動します。
NTのコントロールパネルで設定したのならば、ログオンしなおします。
そうでないならば、Muleを再起動するか、.emacsを評価しなおします。

さぁ、後は実行(M-x trr)あるのみです!!



『実行ファイルを再コンパイルする場合』

まずは以下の2点でゴメンなさいしておきます。
  ・Intel環境も含め、特にMakefileは用意していません。
  ・本当にIntel環境以外でもコンパイルが通るかは不明です。

Cのコードは、VC++4.0でコンパイルできるようにしたつもりです。
コンソール・ウィンドウなどから、以下のようにしてコンパイルしてください。
  cl trr_format.c -DWIN32 -DHAVE_STRING_H -DHAVE_FCNTL_H \
                  -DUSE_TRRDIR -DNO_GREP
  cl trr_update.c -DWIN32 -DHAVE_STRING_H -DHAVE_FCNTL_H \
                  -DUSE_TRRDIR
('\'は、継続行をあらわしています。)



『お願い』

Win32バージョンでのバグは、元バージョンの作者(山本泰宇さん)ではなく、
<yamagus@kw.netlaputa.or.jp>にお願いします。

なお、再配布については、GPLに従います。詳しくは、trr.el.baseを見て下さい。
