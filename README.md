<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 3</b><br/>
"Функціональний і імперативний підходи до роботи зі списками"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Марчук Дмитро КВ-22</p>
<p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання
Реалізуйте алгоритм сортування чисел у списку двома способами: функціонально і
імперативно.
1. Функціональний варіант реалізації має базуватись на використанні рекурсії і
конструюванні нових списків щоразу, коли необхідно виконати зміну вхідного
списку. Не допускається використання: деструктивних операцій, циклів, функцій
вищого порядку або функцій для роботи зі списками/послідовностями, що
використовуються як функції вищого порядку. Також реалізована функція не має
бути функціоналом (тобто приймати на вхід функції в якості аргументів).
2. Імперативний варіант реалізації має базуватись на використанні циклів і
деструктивних функцій (псевдофункцій). Не допускається використання функцій
вищого порядку або функцій для роботи зі списками/послідовностями, що
використовуються як функції вищого порядку. Тим не менш, оригінальний список
цей варіант реалізації також не має змінювати, тому перед виконанням
деструктивних змін варто застосувати функцію copy-list (в разі необхідності).
Також реалізована функція не має бути функціоналом (тобто приймати на вхід
функції в якості аргументів).
Алгоритм, який необхідно реалізувати, задається варіантом (п. 3.1.1). Зміст і шаблон
звіту наведені в п. 3.2.
Кожна реалізована функція має бути протестована для різних тестових наборів. Тести
мають бути оформленні у вигляді модульних тестів (наприклад, як наведено у п. 2.3).

## Варіант 5
Алгоритм сортування обміном №4 ("шейкерне сортування") за незменшенням.

## Лістинг функції з використанням конструктивного підходу
```lisp
(defun bubble-up (lst)
  "Прохід зліва направо: переміщує максимум в кінець списку"
  (cond ((null (cdr lst)) lst)
        (t (let ((x (car lst))
                 (y (cadr lst)))
             (if (> x y)
                 ;; Свап: y стає першим, x йде далі в рекурсію
                 (cons y (bubble-up (cons x (cddr lst))))
                 ;; Порядок вірний: залишаємо x, йдемо далі
                 (cons x (bubble-up (cdr lst))))))))

(defun bubble-down (lst)
  "Прохід справа наліво: переміщує мінімум на початок списку"
  (cond ((null (cdr lst)) lst)
        (t (let* ((res (bubble-down (cdr lst))) ;; Спочатку обробляємо хвіст
                  (x (car lst))
                  (y (car res)))
             (if (> x y)
                 (cons y (cons x (cdr res))) ;; Свап: мінімум (y) наперед
                 (cons x res))))))           ;; Без змін

(defun shaker-sort-functional (lst)
  "Головна функція: комбінує проходи і рекурсивно звужує межі"
  (cond ((or (null lst) (null (cdr lst))) lst)
        (t (let* ((up (bubble-up lst))         ;; 1. Прохід вгору
                  (max-val (car (last up)))    ;;    Максимум
                  (rest-up (butlast up))       ;;    Список без максимуму
                  (down (bubble-down rest-up)) ;; 2. Прохід вниз на залишку
                  (min-val (car down))         ;;    Мінімум
                  (mid (cdr down)))            ;;    Середина
             ;; 3. Конструюємо: мін + (рекурсія середини) + макс
             (cons min-val (append (shaker-sort-functional mid) (list max-val)))))))
```

### Тестові набори
```lisp
(defun check-sort (name func input expected)
  "Утиліта для перевірки результату сортування."
  (let ((result (funcall func input)))
    (format t "~:[FAILED~;passed~]... ~a~%Expected: ~a~%Actual:   ~a~%~%"
            (equal result expected) name expected result)))

(defun test-shaker-functional ()
  (format t "--- Functional Sort Tests ---~%")
  (check-sort "Test 1" #'shaker-sort-functional '(3 1 4 1 5 9 2) '(1 1 2 3 4 5 9))
  (check-sort "Test 2" #'shaker-sort-functional '(5 4 3 2 1) '(1 2 3 4 5))
  (check-sort "Test 3" #'shaker-sort-functional '(10) '(10)))
```
### Тестування
```lisp
CL-USER> (test-shaker-functional)
--- Functional Sort Tests ---
passed... Test 1
Expected: (1 1 2 3 4 5 9)
Actual:   (1 1 2 3 4 5 9)

passed... Test 2
Expected: (1 2 3 4 5)
Actual:   (1 2 3 4 5)

passed... Test 3
Expected: (10)
Actual:   (10)
```
## Лістинг функції з використанням деструктивного підходу
```lisp
(defun shaker-sort-imperative (lst)
  ;; Обов'язково робимо копію, щоб не псувати оригінальні дані
  (let ((res (copy-list lst))
        (left 0)
        (right (1- (length lst)))
        (swapped t))
    ;; Головний цикл поки межі не зійшлись і є обміни
    (loop while (and swapped (< left right)) do
      (setf swapped nil)
      
      ;; Прохід зліва направо
      (loop for i from left below right do
        (when (> (nth i res) (nth (1+ i) res))
          (rotatef (nth i res) (nth (1+ i) res)) ; Деструктивний обмін
          (setf swapped t)))
      (decf right) ; Зменшуємо праву межу
      
      ;; Прохід справа наліво (тільки якщо були зміни)
      (when swapped
        (setf swapped nil)
        (loop for i from right above left do
          (when (< (nth i res) (nth (1- i) res))
            (rotatef (nth i res) (nth (1- i) res)) ; Деструктивний обмін
            (setf swapped t)))
        (incf left))) ; Збільшуємо ліву межу
    res))
```

### Тестові набори
```lisp
(defun test-shaker-imperative ()
  (format t "--- Imperative Sort Tests ---~%")
  (check-sort "Test 1" #'shaker-sort-imperative '(3 1 4 1 5 9 2) '(1 1 2 3 4 5 9))
  (check-sort "Test 2" #'shaker-sort-imperative '(5 4 3 2 1) '(1 2 3 4 5))
  (check-sort "Test 3" #'shaker-sort-imperative '(10) '(10)))
```

### Тестування
```lisp
CL-USER> (test-shaker-imperative)
--- Imperative Sort Tests ---
passed... Test 1
Expected: (1 1 2 3 4 5 9)
Actual:   (1 1 2 3 4 5 9)

passed... Test 2
Expected: (1 2 3 4 5)
Actual:   (1 2 3 4 5)

passed... Test 3
Expected: (10)
Actual:   (10)
```
