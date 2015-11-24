The M Programming Language
========
`m.ml` 파일의 `M.run` 함수를 완성해서 `m.ml` 파일만 제출합니다.

- [숙제문서](http://ropas.snu.ac.kr/~kwang/4190.310/15/hw6.pdf)
- [공지](https://ropas.snu.ac.kr/phpbb/viewtopic.php?t=5157&sid=dab9401469898e5efa942d4f30c29f00)
- [The M Programming Language](http://ropas.snu.ac.kr/~kwang/4190.310/14/M.pdf)

### 컴파일 및 실행 방법

    $ make
    $ ./run examples/test1.m

실행시, 파일명을 명시하지 않을 경우, 표준입력으로부터 실행코드를 읽어들입니다.
표준 입력으로 프로그램을 입력하신 후, 첫 번째 칸(column)에서
유닉스 머신에서는 Ctrl-D, 윈도우 환경에서는 Ctrl-Z를 누르시면
프로그램이 실행됩니다.

### 파스 트리 출력하기

아래와 같이 하면 파싱된 구문구조를 출력한 다음 실행합니다.

    $ ./run -pp examples/test1.m

### 참고 사항

`m.ml`에 M의 문법 및 타입, 실행기, 타입검사기, 구문구조 출력 등의
정의가 모여있으니 한 번쯤 살펴보는 것이 좋겠습니다.

--------

* 04 신재호 <netj@ropas.snu.ac.kr>
* 05 김덕환 <dk@ropas.snu.ac.kr>
* 05 오학주 <pronto@ropas.snu.ac.kr>
*    박대준 <pudrife@ropas.snu.ac.kr>
* 06 이희종 <ihji@ropas.snu.ac.kr>
* 08 최원태 <wtchoi@ropas.snu.ac.kr>
* 09 허기홍 <khheo@ropas.snu.ac.kr>
* 09 김희정 <hjkim@ropas.snu.ac.kr>
* 10 조성근 <skcho@ropas.snu.ac.kr>
* 10 장수원 <swjang@ropas.snu.ac.kr>
* 11 윤용호 <yhyoon@ropas.snu.ac.kr>
* 11 김진영 <jykim@ropas.snu.ac.kr>
*    이승중 <sjlee@ropas.snu.ac.kr>
* 15 최재승 <jschoi@ropas.snu.ac.kr>
