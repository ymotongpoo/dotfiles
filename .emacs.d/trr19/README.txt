TRR19 for Win32 �̐�����

�w����͉�?�x

TRR19 version 1.0 beta5�����Mule for Win32�����ɈڐA�������̂ł��B
TRR���g�̐����́AREADME.euc��info�ɏ���܂��B
README.euc�ɏ]�������@�ŃC���X�g�[������΁AUNIX��ł����삵�܂��B



�w������x

���s�ɂ́A���R�A�uMule for Win32�v���K�v�ł��B
Intel���ł���΁A�킸���Ȑݒ�œ���\�Ǝv���܂��B
Intel���ȊO�ł́Atrr_format.exe��trr_update.exe���ăR���p�C������K�v
������܂��B

1997/3/18���݁A����m�F�̂Ƃ�Ă�����́A�ȉ��̒ʂ�ł��B
  �EWindows95��W113�̑g�ݍ��킹
  �EWindowsNT4.0 Workstation(Intel��)��W113�̑g�ݍ��킹



�w�t�@�C����W�J����x

���̃t�@�C����ǂ�ł���Ȃ�΁A���łɓW�J�͍ς�ł��邱�Ƃł��傤�B
WinZip�œW�J�����ꍇ�A�uTAR File Smart CR/LF Conversion�v�I�v�V�������I
�t�ɂȂ��Ă��邱�Ƃ��m�F�����ق���������������܂���B(�ǂ��Ȃ񂾂낤?)

�W�J����ƁAtrr19�Ƃ����f�B���N�g�����쐬����A���̒��ɂ������̃t�@�C
��(�ƃT�u�f�B���N�g��)���ł��܂��B

�ȍ~�ɂ������́A�ȉ��̊���z�肵�܂��B
  �E���ϐ�HOME�̒l�́Ac:\home\foo�ɐݒ肳��Ă���B
  �Ec:\home\foo�ɓW�J���Ac:\home\foo\trr19���쐬���ꂽ�B



�w���ϐ���ݒ肷��x

TRRDIR��TRRBINDIR�́A2�̊��ϐ��̐ݒ肪�K�v�ł��B

  TRRDIR:
    CONTENTS�t�@�C��, text�T�u�f�B���N�g�������record�T�u�f�B���N�g����
    �܂ރf�B���N�g�����A�t���p�X�Ŏw�肵�܂��B
    ����3���Z�b�g�ŁA�ʂ̏ꏊ�ɒu�����Ƃ��o���܂��B
    �f�B���N�g����؂���X���b�V��('/')�ŏ����Ă����v�ł��c�����B
    ��ł̏ꍇ�́uc:\home\foo\trr19�v�ł��B

  TRRBINDIR:
    ���s�t�@�C��(trr_format.exe��trr_update.exe)���܂ރf�B���N�g�����A
    �t���p�X�Ŏw�肵�܂��B
    �f�B���N�g����؂���X���b�V��('/')�ŏ����Ă����v�ł��B
    ��ł̏ꍇ���ƁA��������uc:\home\foo\trr19�v�ł��B
    ���s�t�@�C��������ʂ̏ꏊ�ɒu�����Ƃ��o���܂��B

.emacs�̒��Ŏw�肷��ꍇ�A�ȉ��̂悤�ɐݒ肵�܂��B
  (setenv "TRRDIR" "c:/home/foo/trr19"))
  (setenv "TRRBINDIR" "c:/home/foo/trr19"))

�ȉ��̂悤�ɂ��������Ƃ��ł��܂��B
  (setenv "TRRDIR" (expand-file-name "~/trr19"))
  (setenv "TRRBINDIR" (expand-file-name "~/trr19"))



�wMule��trr�̂��肩��������x

.emacs��(autoload ...)�������܂��B
��ł̏ꍇ���ƁA�ȉ��̂悤�ɂȂ�܂��B
  (autoload 'trr "c:/home/foo/trr19/trr" nil t)

�ȉ��̂悤�ɂ��������Ƃ��ł��܂��B
  (autoload 'trr (expand-file-name "~/trr19/trr") nil t)



�w�N���̑O�Ɂx

�K��README.euc��info�ɖڂ�ʂ��Ă��������B
�����́A���ɕύX�������Ă��܂���B
�֑��Ȃ���Ainfo�́A�uC-u M-x info�v���āAtrr.info���w�肷��Γǂ߂܂��B

���̏��trr.el�ɕύX���K�v�ȏꍇ�́A�ύX�������Ă��������B
(�l�I�ɂ́ATRR:installator��ύX���Ăق����ł��B ^^;)

�o�C�g�E�R���p�C�����s���Ă����܂��܂���B

���ϐ���AUTOEXEC.BAT�Ŏw�肵���Ȃ�΁AWindows���ċN�����܂��B
NT�̃R���g���[���p�l���Őݒ肵���̂Ȃ�΁A���O�I�����Ȃ����܂��B
�����łȂ��Ȃ�΁AMule���ċN�����邩�A.emacs��]�����Ȃ����܂��B

�����A��͎��s(M-x trr)����݂̂ł�!!



�w���s�t�@�C�����ăR���p�C������ꍇ�x

�܂��͈ȉ���2�_�ŃS�����Ȃ������Ă����܂��B
  �EIntel�����܂߁A����Makefile�͗p�ӂ��Ă��܂���B
  �E�{����Intel���ȊO�ł��R���p�C�����ʂ邩�͕s���ł��B

C�̃R�[�h�́AVC++4.0�ŃR���p�C���ł���悤�ɂ�������ł��B
�R���\�[���E�E�B���h�E�Ȃǂ���A�ȉ��̂悤�ɂ��ăR���p�C�����Ă��������B
  cl trr_format.c -DWIN32 -DHAVE_STRING_H -DHAVE_FCNTL_H \
                  -DUSE_TRRDIR -DNO_GREP
  cl trr_update.c -DWIN32 -DHAVE_STRING_H -DHAVE_FCNTL_H \
                  -DUSE_TRRDIR
('\'�́A�p���s������킵�Ă��܂��B)



�w���肢�x

Win32�o�[�W�����ł̃o�O�́A���o�[�W�����̍��(�R�{�׉F����)�ł͂Ȃ��A
<yamagus@kw.netlaputa.or.jp>�ɂ��肢���܂��B

�Ȃ��A�Ĕz�z�ɂ��ẮAGPL�ɏ]���܂��B�ڂ����́Atrr.el.base�����ĉ������B
