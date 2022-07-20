# 背景

https://zenn.dev/zahn/articles/948fc5b66648a3

この記事で提案されていた”複数の営業日カレンダーを扱う業務のための日付モデル”が素晴らしいと思ったので実装してみました！

## 環境
saclaとscala-parser-combinatorsを利用します
```
scalaVersion := "2.13.8"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1"
```

## 複数の営業日カレンダーを扱う業務のための日付モデルとは
簡単に説明をする。

## まずはEBNF通りに実装する

以下が元記事で提案されている”複数の営業日カレンダーを扱う業務のための日付モデル”のEBNFです。
```
binop    ::= '+'
           | '-'
castop   ::= '_'
           | '^'
cal      ::= 'jp'
           | 'us'
           | 'c'
           | 'jp & us'
           | 'jp | us'
expr     ::= ( ( "T" | expr ) cast_op ) cal | ( "(" expr ")" ) | expr binop num
```

まずはこのEBNFの規則に従って実装してみます。以下が実装例です。
```
object BusinessCalendarParser extends JavaTokenParsers with RegexParsers {
  private def binop = "+" | "-"
  private def castop = "_" | "^"
  private def cal: Parser[String] = "jp&us" | "jp|us" | "jp" | "us" | "c"
  private def integer = wholeNumber
  private def T: Parser[String] = """[0-9]{4}/[0-9]{2}/[0-9]{2}""".r ^^ { _.toString } //
  private def expr: Parser[Any] = ((T | expr) ~ castop ~cal) | ("(" ~ expr ~ ")") | (expr ~ binop ~ interger)

  def apply(input: String): Either[String, Any] = parseAll(expr, input) match {
    case Success(c, n) => Right(c)
    case Failure(e, _) => Left("FAILURE: " + e.toString)
    case Error(e, _) => Left("ERROR: " + e.toString _)
  }
}
```

このパーサーを実行しようとすると入力する文字のパターンによってはスタックオーバーフローになってしまいます。

```
例外になる入力例と例外の出力を貼る
```

色々イジってみた結果、これはscala-parser-combinatorsの制約で”左再帰を使った生成規則を避ける必要がある”を満たしておらず、左再帰になるような規則でパーサー作っていたから起きていました。

## 変更したEBNFとその実装
”複数の営業日カレンダーを扱う業務のための日付モデル”をパースするには記事で提案されていたENBFそのままではなく元のEBNFと同じ文字列を評価でき、かつLL(1)になるような定義が必要です。

少し妥協しましたが、以下が私が修正したENBFの定義です。

```
binop    ::= '+'
           | '-'
castop   ::= '_'
           | '^'
cal      ::= 'jp'
           | 'us'
           | 'c'
           | 'jp&us'
           | 'jp|us'
expr     ::= term cast_op cal [ binop num ]
term     ::= "(" expr ")" | "T"
```

新たにtermという項を定義し、exprとtermの２つを使って元のEBNFのexprとほぼ同様の定義ができました。

新たなこの規則だと"(" "T" cast_op cal ")" は評価できなくなります。
例えば "(2020/12/26^jp)", "((2020/12/26^jp)))" などが評価できなくなります。()があっても評価結果は変わらないはずなので今回は()で囲むだけのtokenは規則に含めないことにしました。
※後もうひとつ扱いにくいので jp & us -> jp&us,  jp | us -> jp|us とスペースを削除しています。

以下が修正したEBNFの定義通りにパーサーを実装した例です。
```
object BusinessCalendarParser extends JavaTokenParsers with RegexParsers {
  private def binop = "+" | "-"
  private def castop = "_" | "^"
  private def cal: Parser[String] = "jp&us" | "jp|us" | "jp" | "us" | "c"
  private def integer = wholeNumber
  private def T: Parser[String] = """[0-9]{4}/[0-9]{2}/[0-9]{2}""".r ^^ { _.toString }
  private def expr: Parser[Any] = term ~! castop ~! cal ~! (binop ~! integer).?
  private def term: Parser[Any] = ("(" ~! expr ~! ")") | T

  def apply(input: String): Either[String, Any] = parseAll(expr, input) match {
    case Success(c, n) => Right(c)
    case Failure(e, _) => Left("FAILURE: " + e.toString)
    case Error(e, _) => Left("ERROR: " + e.toString _)
  }
}
```

## case classへの変換
規則に従ってパースができることがわかったのでパース結果を扱いやすいようにcase classに変換します

まずはTokenを定義します
```
object Token {
  sealed trait BinOp
  case object Plus extends BinOp
  case object Minus extends BinOp

  sealed trait CastOp
  case object Hat extends CastOp
  case object UnderBar extends CastOp

  sealed trait Cal
  case object JP extends Cal
  case object EN extends Cal
  case object Center extends Cal //ここcはCenterだと思っていたらCalendarの略のcだった。Calendar case classはもう作ってしまったのでどう名前を変えるか迷い中
  case object JPAndEN extends Cal
  case object JPOrEn extends Cal

  case class Num(int: Int)

  sealed trait Expr
  case class BusinessDayCalendar(
      expr: Expr,
      castOp: CastOp,
      cal: Cal,
      maybeBinOp: Option[BinOp],
      maybeInt: Option[Num]
  ) extends Expr
  
    case class Calendar(localDate: LocalDate) extends Expr
  object Calendar {
    def fromString(date: String): Calendar = Calendar(
      LocalDate.parse(
        date,
        DateTimeFormatter
          .ofPattern("uuuu/MM/dd")
          .withResolverStyle(ResolverStyle.STRICT)
      )
    )
  ```

次にパース部分でパースが成功した場合に逐次該当するcase classに変換をするように変換を行います
以下が実装例です。

```
import businesscalendarparser.Token._
object BusinessCalendarParser extends JavaTokenParsers with RegexParsers {
  private def binop: Parser[BinOp] = ("+" | "-") ^^ {
    case "+" => Plus
    case "-" => Minus
  }

  private def castop: Parser[CastOp] = ("_" | "^") ^^ {
    case "_" => UnderBar
    case "^" => Hat
  }

  private def cal: Parser[Cal] = ("jp&us" | "jp|us" | "jp" | "us" | "c") ^^ {
    case "jp&us" => JPAndEN
    case "jp|us" => JPOrEn
    case "jp"    => JP
    case "us"    => EN
    case "c"     => Center
  }

  private def integer = wholeNumber ^^ { s => Num(s.toInt) }

  private def T: Parser[Calendar] =
    """[0-9]{4}/(0[1-9]|1[0-2])/(0[1-9]|[12][0-9]|3[01])""".r.filter(
      Calendar.canCreateCalendarInstance
    ) ^^ Calendar.fromString

  private def expr: Parser[BusinessDayCalendar] =
    term ~! castop ~! cal ~! (binop ~! integer).? ^^ {
      case (t: Expr) ~ (cas: CastOp) ~ (cl: Cal) ~ Some(
            (b: BinOp) ~ (i: Num)
          ) =>
        BusinessDayCalendar(t, cas, cl, Some(b), Some(i))
      case (t: Expr) ~ (cas: CastOp) ~ (cl: Cal) ~ None =>
        BusinessDayCalendar(t, cas, cl, None, None)
    }

  private def term: Parser[Expr] = ("(" ~>! expr <~! ")") | T

  def apply(input: String): Either[String, BusinessDayCalendar] =
    parseAll(expr, input) match {
      case Success(c, n) => Right(c)
      case Failure(e, _) => Left("FAILURE: " + e.toString)
      case Error(e, _)   => Left("ERROR: " + e.toString)
   
```

この実装で元記事で紹介されていた"((2020/12/26_c+1)^jp+1)_us-1"のパースが可能なことがわかります。以下が該当のテストです。

```
  it should "((2020/12/26_c+1)^jp+1)_us-1" in {
      val bcp = BusinessCalendarParser("((2020/12/26_c+1)^jp+1)_us-1")
      bcp shouldBe a [Right[_, _]]
      bcp.right.get shouldBe BusinessDayCalendar(BusinessDayCalendar(BusinessDayCalendar(Calendar(LocalDate.of(2020, 12, 26)), UnderBar, Center, Some(Plus), Some(Num(1))), Hat, JP, Some(Plus), Some(Num(1))), UnderBar, EN, Some(Minus), Some(Num(1)))
  }  
```


"((2020/12/26_c+1)^jp+1)_us-1"が((2020/12/26の翌日)から翌日以降のJPX営業日の翌日の営業日)から翌日以前のEN営業日の前日の営業日)
と表現できているのがわかります。

次はこのcase classから結果の営業日を導き出す必要があります
## 評価
デモのためにAとBとCを用意します。今回がデモのためにかなり雑な実装をしており、実運用に耐えられる実装ではありません。つまりうんこーどです。

評価をするためには日本の営業日とアメリアかの営業日を読み出せる形として永続化する必要があります。
実際の市場の営業日を調べるのは面倒なので2020/12/21-2021/1/8 までを祝日などを考慮して仮に以下のように定義しました
![](https://storage.googleapis.com/zenn-user-upload/7117855d7f68-20220720.png)

またBusinessDayCalendarから日付を実装取得する実装例は以下になります。

```
コードを貼る
```

このコードの実行結果が以下になります。

```
input: 2020/12/22^jp+1
token: Right(BusinessDayCalendar(Calendar(2020-12-22),Hat,JP,Some(Plus),Some(Num(1))))
result: Some(2020-12-24)
```

2020/12/22の翌日の日本の営業日は2020/12/22の翌日の2020/12/23は祝日なのでその翌日の2020/12/24となることがわかります。

元記事にでていた"((2020/12/26_c+1)^jp+1)_us-1"は

```
結果を書く
```

と正しく結果を出力できるようになりました。


評価の途中でにDBアクセスをするのがモヤモヤする実装でもっと良い方法があると思います。
sqlliteでミドルウェア的な扱いにして読み込めば良いのですかね...?

## まとめ
- ”複数の営業日カレンダーを扱う業務のための日付モデル”の構文解析器、評価機を実装しました
- 既存の定義ではパーサーコンビネーターで扱いにくいので修正を加えた

## Reference
- https://github.com/YuMuuu/BusinessDayModel
- コップ本3版 33.10 バックトラックとLL(1)



