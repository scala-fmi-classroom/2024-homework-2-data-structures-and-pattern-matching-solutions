краен срок: 17.04.2024
---
# Структури от данни и pattern matching

В това домашно ще упражним наученото за неизменими и lazy структури от данни и pattern matching от предните лекции.

В следващото домашно ще използваме една от имплементираните тук структури от данни (`Chain`) в имплементацията на ефект за валидация на вход, където ще се сблъскваме с решаването на проблем в конкретен бизнес домейн и ще изградим малко по-цялостно приложение.

## Непразни последователности. `Chain` (4,5 точки)

В курса разгледахме как тоталните функции ни помагат да се предпазим от неочаквани ситуации. За да постигнем това, често разширяваме резултатния тип за да опишем тези гранични ситуации – например връщаме `Option`, `Try`, `Either` или нещо друго.

Това обаче може да направи работата за клиентите на тези функции по неудобна, например:

```scala
def average(numbers: List[Double]): Option[Double] =
  if numbers.isEmpty then None
  else Some(numbers.sum / numbers.size)

average(List(2, 3, 7)).getOrElse(sys.error("Unexpected"))
``` 

В случая клиентът знае, че списъкът не е празен и притежава средно аритметично, но типовата система по никакъв начин не му помага за това и му налага да се справи и със случая на подаден празен списък. Понякога в такива ситуации е възможно да елиминираме тези невъзможни състояния, така че да не е необходимо да се грижим за тях, ограничавайки наборът от възможните входни аргументи на функциите. Ако вместо `List` приемем тип `NonEmptyList`, то функцията би изглеждала така:

 ```scala
 def average(numbers: NonEmptyList[Double]): Double = numbers.sum / numbers.size
 
 average(NonEmptyList(2, 3, 7))
 ```

Това много по-ясно и директно описва проблемът, който решаваме.

Първата част от това домашно е да имплементираме такава непразна последователност. Вместо чрез стандартен свързан списък обаче, ще извършим имплементацията чрез структура, оптимизирана за друг тип операции – слепване на последователности. Ще наричаме този тип `Chain`. Ще искаме слепването на две `Chain` инстанции да е с константна сложност, за сметка на други стандартни операции, като извличане на първи елемент и достъп до остатъка от верига, които ще се наложи да се превърнат в линейни в най-лошия случай.

Доста често във функционални трансформации ни се налага да слепваме междинни последователности, които получаваме, след което линейно да обходим и трансформираме целия резултат. Именно в тези случаи би бил полезен `Chain`.

Дефинираме `Chain` като сума на два подтипа: верига от един елемент (`Singleton`) и слепване на две вериги (`Append`):

```scala
enum Chain[+A]:
  case Singleton(a: A)
  case Append(left: Chain[A], right: Chain[A])
```

---
**Отклонение:**

В enum-ите Scala автоматично разпознава какво желаем да наследим и параметрите, които имаме, поради което горният синтаксис е напълно еквивалентен на следния:

```scala
enum Chain[+A]:
  case Singleton[A](a: A) extends Chain[A]
  case Append[A](left: Chain[A], right: Chain[A]) extends Chain[A]
```

Алтернативен, и в някои ситуации по-гъвкав начин да се запише сумиращ тип, е чрез `sealed` йеархия, като при нея сме задължени да опишем напълно типовете:

```scala
sealed trait Chain[+A]
case class Singleton[A](head: A) extends Chain[A]
case class Append[A](left: Chain[A], right: Chain[A]) extends Chain[A]
```

---

В предоставения ви код някои от операциите, като `head`, `tail` и `isEmpty`, са вече реализирани за вас. Вашата задача е да допълните имплементация със следните операции за `Chain`:

* `def +:[B >: A](front: B): Chain[B]` – добавяне на елемент в началото
* `def :+[B >: A](back: B): Chain[B]` – добавяне на елемент в края
* `def ++[B >: A](right: Chain[B]): Chain[B]` – слепване две последователности (`Chain(1, 2) ++ Chain(3, 4) == Chain(1, 2, 3, 4)`)
* Операции `foldLeft`, `map`, `flatMap` със вече познатата ни семантика
* `def listify: Chain[A]`, която връща нова верига с променена структура, но стойностно равняваща се на оригиналната последователност. Променената структура наподобява структурата на свързания списък и изискването е всеки ляв елемент на `Append` верига да бъде `Singleton`.

  Например 
  
  ```scala
  Append(Append(Singleton(1), Singleton(2)), Append(Singleton(3), Singleton(4))).listify
  ```
  
  връща 
  
  ```scala
  Append(Singleton(1), Append(Singleton(2), Append(Singleton(3), Singleton(4))))
  ```
  
  При тази структура на веригата операциите `head` и `tail` стават константни.
* `def apply[A](head: A, rest: A*): Chain[A]` върху companion обекта `Chain` – създава верига с първи елемент `head` и последователността `rest` за оставащите елементи.

Автоматично генерираните от `case` класовете методи `equals` и `hashCode` в този случай няма да са ни полезни, тъй като две вериги с различна структура, но със същата последователност от елементи, също трябва да бъдат равни. Затова предоставяме собствена имплементация и на тези методи.

В предоставения ви код е добавена имплементация на `hashCode`, която добре разпределя възможните върнати стойности (възползвайки се от свойствата на простите числа, в случая 31). От вас се изисква да имплементирате метода `equals`.

1(__*__) от точките за тази задача са с леко повишена трудност и ще ви я дадем, ако имплементирате `foldLeft` опашково-рекурсивно. Останалите операции не е нужно да са опашково-рекурсивни, но, ако ви е любопитно, е добро упражнение да помислите как и дали биха могли да станат.

**Забележка 1:** забележете, че операцията `tail` е имплементирана чрез `listify`. Затова бъдете внимателни да не използвате `tail` във вашата имплементация на `listify.

**Забележка 2:** ако за имплементацията на `foldLeft` използвате функция, която не е опашкова рекурсивна, то самият `foldLeft` също няма да ви го счетем за опашково-рекурсивен, тъй като поради използването на другата функция има опасност да се препълни стека. Например ако използвате `tail` то най-вероятно вашият `foldLeft` няма да е опашково-рекурсивен.

## Lazy редици (2 точки)

На лекциите имплементирахме `LazyList`. Тук може да откриете тази имплементация, генерилизирана за произволен тип на елементите и имплементираща `LazyList` като ADT.

Вашата задача ще е да добавите всичко необходимо към `LazyList`, така че примерът за фибоначи от лекциите да заработи успешно:

```scala
val fibs: LazyList[Long] = 0L #:: 1L #:: (fibs zip fibs.tail).map(_ + _)
val firstTenFibs = fibs.take(10).toList // List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
```

Нужно е да имплементирате всички липсващи операции (`zip`, `map`, `take`, `toList`). Помислете дали и кои от тях трябва да бъдат опашково-рекурсивни и за кои всъщност lazy изчислението позволява те да не са опашкови (и дори прави имплементирането им като такива невъзможно), както и защо това е така.

Втората част от тази секция е да имплементирате безкрайна редица самите вие – тази на всички факториели, започвайки от 0!:

```scala
val factorials: LazyList[Long] = ???
val firstTenFactorials = factorials.take(10).toList // List(1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880)
```

## Сортиране и pattern matching (1,5 точки)

Както научихме от лекциите, pattern matching ни позволява много лесно да деструктурираме обекти на частички, които след това да трансформираме. Така лесно можем например да имплементираме `quickSort`:

```scala
def quickSort[A](as: List[A])(isSmaller: (A, A) => Boolean): List[A] = as match
  case Nil => Nil
  case a :: rest =>
    val (smaller, bigger) = rest.partition(isSmaller(_, a))

    quickSort(smaller)(isSmaller) ::: (a :: quickSort(bigger)(isSmaller))
```

Вашата задача е да напишете имплементация за сортиране по алгоритъма "merge sort":

```scala
def mergeSort[A](as: List[A])(isSmaller: (A, A) => Boolean): List[A] = ???

mergeSort(List(4, 1, 0, 10, -5, 30))(_ < _) // List(-5, 0, 1, 4, 10, 30)
```

Помислете как може да се възползвате най-добре от pattern matching при сливането на два сортирани списъка в един.

## Стил (2 точки*)

Това, което също ще искаме да оценим в тази задача, е стилът, който ще приложете във вашето решение.

За изключително добри решения ще получите и допълнително над тези 2 точки, Аналогично, при лош стил и ненужно използване на странични ефекти, `var` или `return`, си запазваме правото и да отнемем точки.

## Форматиране

За да ви насочим към консистентен стил към домашно отново сме добавили scalafmt конфигурация. За да форматирате кода си може да използвате scalafmt командата на sbt (идваща от sbt-scalafmt plugin-а, който сме добавили) или scalafmt cli инструмента, който се инсталира автоматично при вашата инсталация на Scala от Coursier.

Вижте документацията на scalafmt за как да интегрирате и вашите IDE или редактор. Препоръчваме да пуснете опцията за форматиране при запазване на файл.

В `.scalafmt.conf` сме добавили наша конфигурация за формат. Чувствайте се напълно свободни да я промените, ако предпочитате различен стил. Важното е накрая кодът да е консистентен.

## Допълнителни указания и оценяване

Използвайте проекта, предоставен в тази директория. Насърчаваме ви да добавите собствени тестове, за да сте сигурни, че вашата имплементация е правилна.

Също, за да можем да изпълним и нашите тестове, моля предайте код, който се компилира, и при който не сте променили предоставения интерфейс. Ако не сте имплементирали някоя от функциите, моля все пак да не я изтривате, тъй като иначе ще видим компилационна грешка. 

Цялостното решения на задачата ви носи 10 точки.

## Напътствия

* Pattern matching-ът е изключително полезен при структурни трансформации.
* Когато имплементирате нова операция на `Chain` не забравяйте да помислите дали тя не може да бъде изразена чрез някоя от другите.
* Не се притеснявайте да ни питате всякакви въпроси.
