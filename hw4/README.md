짱멋진 보물섬 자동채점기
========

이쁘고 보기 쉬운 PL 4번과제 자동 채점기

```console
curl https://raw.githubusercontent.com/simnalamburt/snucse.pl/master/hw4/test -O
ocaml test
```

![](http://i.imgur.com/MQ7TxU2.png)

틀리는 테스트케이스에 대해선, 문제 문제와 일대일 대응되는 람다 익스프레션,
그리고 모범답안을 나란히 출력하여서, 학생이 간편하게 자신의 코드를 디버깅할 수
있도록 해줍니다.

![](http://i.imgur.com/UvtE9py.png)

--------

### References
* [LEE, Oukseh; YI, Kwangkeun. Proofs about a folklore let-polymorphic type
  inference algorithm. ACM Transactions on Programming Languages and Systems
  (TOPLAS), 1998, 20.4: 707-723.](http://ropas.snu.ac.kr/~kwang/paper/98-toplas-leyi.pdf)
