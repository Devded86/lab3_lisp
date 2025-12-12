;;; 1. ФУНКЦІОНАЛЬНА РЕАЛІЗАЦІЯ (Конструктивний підхід)
;;; Використовує рекурсію і створення нових списків (cons).

(defun bubble-up (lst)
  "Прохід зліва направо: переміщує максимум в кінець списку."
  (cond ((null (cdr lst)) lst)
        (t (let ((x (car lst))
                 (y (cadr lst)))
             (if (> x y)
                 ;; Міняємо місцями: y стає першим, x йде далі в рекурсію
                 (cons y (bubble-up (cons x (cddr lst))))
                 ;; Порядок вірний: залишаємо x, йдемо далі
                 (cons x (bubble-up (cdr lst))))))))

(defun bubble-down (lst)
  "Прохід справа наліво: переміщує мінімум на початок списку."
  (cond ((null (cdr lst)) lst)
        (t (let* ((res (bubble-down (cdr lst))) ; Спочатку обробляємо хвіст
                  (x (car lst))
                  (y (car res)))
             (if (> x y)
                 (cons y (cons x (cdr res))) ; Свап: мінімум (y) наперед
                 (cons x res))))))           ; Без змін

(defun shaker-sort-functional (lst)
  "Головна функція: комбінує проходи і рекурсивно звужує межі."
  (cond ((or (null lst) (null (cdr lst))) lst) ; Базовий випадок (0 або 1 елемент)
        (t (let* ((up (bubble-up lst))         ; 1. Прохід вгору
                  (max-val (car (last up)))    ;    Отримуємо максимум
                  (rest-up (butlast up))       ;    Відрізаємо максимум
                  (down (bubble-down rest-up)) ; 2. Прохід вниз на залишку
                  (min-val (car down))         ;    Отримуємо мінімум
                  (mid (cdr down)))            ;    Відрізаємо мінімум
             ;; 3. Конструюємо: мін + (рекурсія середини) + макс
             (cons min-val (append (shaker-sort-functional mid) (list max-val)))))))

;;; 2. ІМПЕРАТИВНА РЕАЛІЗАЦІЯ (Деструктивний підхід)
;;; Використовує цикли (loop) і зміну пам'яті (rotatef).

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


;;; 3. ТЕСТУВАННЯ

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

(defun test-shaker-imperative ()
  (format t "--- Imperative Sort Tests ---~%")
  (check-sort "Test 1" #'shaker-sort-imperative '(3 1 4 1 5 9 2) '(1 1 2 3 4 5 9))
  (check-sort "Test 2" #'shaker-sort-imperative '(5 4 3 2 1) '(1 2 3 4 5))
  (check-sort "Test 3" #'shaker-sort-imperative '(10) '(10)))

;;; Запуск тестів:
 (test-shaker-functional)
 (test-shaker-imperative)