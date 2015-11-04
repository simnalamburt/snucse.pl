SM5
========
sm5.ml에는 sm5 모듈이 정의되어 있고, k.ml에는 K-- 인터프리터가 구현되어 있습니다.
아래와 같이 실행하면, 주어진 k-- 프로그램을 여러분이 작성하신 번역기에 따라
번역하고 SM5 기계로 실행합니다.
```console
make
./run examples/test1.k--
```

실행시, 파일명을 명시하지 않을 경우, 표준입력으로부터 실행코드를 읽어들입니다.
표준 입력으로 프로그램을 입력하신 후, 첫 번째 칸(column)에서
유닉스 머신에서는 Ctrl-D, 윈도우 환경에서는 Ctrl-Z를 누르시면
프로그램이 실행됩니다.

* [숙제 문서](http://ropas.snu.ac.kr/~kwang/4190.310/15/hw5.pdf)
* [공지](https://ropas.snu.ac.kr/phpbb/viewtopic.php?t=5128)
* [테스트케이스](https://ropas.snu.ac.kr/phpbb/viewtopic.php?t=5129)
* [K-- 스펙](http://ropas.snu.ac.kr/~ta/4190.310/15/document/K_doc/k--.pdf)

### 숙제 제출 관련
1.  SM5 문제

    translate.ml 파일에 있는 trans 함수를 완성하시고 translate.ml 파일만 제출해 주세요.

2.  SM5 + 메모리 재활용 문제

    sm5.ml 파일에 있는 malloc 함수를 완성하시고 sm5.ml 파일만 제출해 주세요.

### 파스 트리 출력하기
입력 프로그램의 파스 트리를 화면에 출력해주는 모듈이 pp.ml 파일에 포함되어
있습니다. 이를 통해 파싱이 의도한 대로 되고 있는지 확인해 보실 수 있습니다.
```console
./run -pp examples/test1.k--
```

### K-- 실행기로 실행하기
trans가 제대로 정의되었는지 K-- 실행기로 실행한 결과와 비교해 볼 수 있습니다.
```console
./run -k examples/test1.k--
```

--------

* 03 최웅식 <wschoi@ropas.kaist.ac.kr>
* 04 신재호 <netj@ropas.snu.ac.kr>
* 05 김덕환 <dk@ropas.snu.ac.kr>
* 05 오학주 <pronto@ropas.snu.ac.kr>
* 06 이희종 <ihji@ropas.snu.ac.kr>
* 07 오학주 <pronto@ropas.snu.ac.kr>
* 09 허기홍 <khheo@ropas.snu.ac.kr>
* 10 조성근 <skcho@ropas.snu.ac.kr>
* 13 최준원 <jwchoi@ropas.snu.ac.kr>
* 13 강동옥 <dokang@ropas.snu.ac.kr>
* 15 최재승 <jschoi@ropas.snu.ac.kr>
