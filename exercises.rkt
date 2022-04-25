#lang gamble
(require gamble/viz)
(require racket)

;Guido Laudenzi
;0001033343

;Exercise 1

;1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;A query example for studying the compound behaviour of the procedure:

;(a-plus-abs-b (and 3 2) -4)
;6
;(a-plus-abs-b (or 3 2) -4)
;7

;The procedure sums the expression a with the absolute value of b. In fact, if b is
;bigger than 0, it will be a sum, otherwise it will be a subtraction.
;Anyway, it is possible to use the logical compound operations such as and, or and not.
;This is because when the interpreter checks a predicate’s value, it interprets #f
;as false and any other value is treated as true.
;In particular, we have different behaviors depending on or, and and not:
;AND: interpreter evaluates the expressions one at a time, in left-to-right order.
;If any expression evaluates to false, the value of the and expression is false, and
;the rest of the expressions are not evaluated. If all expressions evaluate to true
;values, the value of the and expression is the value of the last one.
;OR: e interpreter evaluates the expressions one at a time, in left-to-right order.
;If any expression evaluates to a true value, that value is returned as the value of
;the or expression, and the rest of the expressions are not evaluated. If all
;expressions evaluate to false, the value of the or expression is false.
;NOT: the value of a not expression is true when the expression evaluates to false,
;and false otherwise. Here, we would end up with an error if using not with a number
;(#f cannot be used in the procedure).

;1.5
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

;The interpreter first evaluates the operator and operands and then applies
;the resulting procedure to the resulting arguments. This is not the only
;way to perform evaluation. An alternative evaluation model would not
;evaluate the operands until their values were needed.
;The first one is the applicative-order evaluation, the second one is known as
;normal-order evaluation.
;The standard evaluation of the interpreter is the first one. Doing so, it firstly
;evaluates the operands then test procedure, so it ends up in a loop while evaluating
;(p), that is a function that evaluates to itself.
;Using normal-order evaluation, we would get 0 as result: this is because
;operands are not needed for evaluating test, so if x is 0 (that is in our query)
;the interpreter would not need to evaluate (p)(that results in a loop) because of the
;if.


;Exercise 2

;1.35
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;We know that x->1+1/x basically means x = 1 + 1/x. It can be transformed in a second
;order equation which has one solution: x = 1 + \sqrt(5)/2. For the section 1.2.2,
;we know that the golder ratio is exactly that quantity and that is equal to 1.6180.
;Fixed-point procedure is given by the book and wants as inputs a function and an
;initial guess. We need to give x = 1+1/x as function input, while the initial guess
;could be whatever number (we already know that the golden ratio is about 1.6, so we
;can already give similar numbers).
;Example: (fixed-point (lambda (x) (+ 1 (/ 1 x))) 3.0)

;1.36
;Firstly, we take the book's function for the average:
(define (average x y)
  (/ (+ x y) 2))

;Then, we modify the fixed-point procedure so to print the guesses:
(define (fixed-point1 f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess) ;here we print each guess
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;Finally, we just need to use queries in order to compare the results with and without
;average damping. We must remember not to use 1.0 as initial guess, as written in the
;exercise, because we are using the primitive log.
;Input function would be log(1000)/log(x); initial guess is the same.
;
;(fixed-point1 (lambda (x) (/ (log 1000) (log x))) 3.0)
;(fixed-point1 (lambda (x) (average x (/ (log 1000) (log x)))) 3.0)
;
;Using average damping would let the procedure run faster, maintaining the same accuracy.
;(remember to print the values with display)

;1.37
(define (cont-frac n d k)
  (define (fraction i)
    (/ (n i) (+ (d i) (if (= k i) 0 (fraction (+ 1 i))))))
  (fraction 1))

;This is the recursive procedure. In particular, it compiles the k-term finite
;continued fraction until the order k which is given by input.
;The cont-frac procedure firstly defines the function fraction, then calls it with
;with starting value equal to 1. It will append to the denominator another fraction
;until the value of i get to the one of k.
;We can get an approximation that is accurate to 4 decimal places (0.6180) if we set
;k to 11 (or higher). Otherwise, the value would not be exact. (screen)

;This is the iterative procedure. The substantial difference is that the intermediate
;result will be carried as parameter at each step. Accuracy is the same as the
;recursive procedure.

(define (cont-frac-it n d k)
  (define (fraction k c)
    (if (= k 0) c (fraction (- k 1) (/ (n k) (+ (d k) c)))))
  (fraction k 0))
    
;1.38
;We know that Di is a regular series except for the first index which has value 1.
;All the other values are 1, except for the indexes 2,5,8, etc. where there are values
; of 2 times table (2,4,6,8,10,etc.). We need to define a procedure to enstablish the
;values of Di based on the indexes.
;We find that Di=2(i+1)/3 where i is the index.
;All the Nis are 1.

(define (d i)
  (if (not(= 0 (remainder (+ i 1) 3))) 1 (* 2 (/ (+ i 1) 3))))
(define (n i) 1.0)
(define (e k)
  (+ 2 (cont-frac n d k)))


;Exercise 3
;2
;(define foo (flip))
;(list foo foo foo)
;(define (foo) (flip))
;(list (foo) (foo) (foo))
;
;The difference is that in the first case the interpreter binds the procedure flip to
;a variable called foo. Therefore, its value is not going to change afterwards and the
;list will display only the same values (#f,#f,#f or #t,#t,#t), that are foos.
;The latter is defined as a procedure which recall the procedure flip. This means that
;everytime we construct a list of (foo), they will have different values because they
;will be generated by the flip procedure.

;5
(define sum (λ (xs) (foldl + 0 xs))) ;from exercises of week 2

(define strength (mem (lambda (person) (if (flip) 5 10))))
(define lazy (lambda (person) (flip (/ 1 3))))
(define (total-pulling team)
  (sum
   (map (lambda (person) (if (lazy person) (/ (strength person) 2) (strength person)))
        team)))
(define (winner team1 team2) (if (< (total-pulling team1) (total-pulling team2)) team2 team1))

;(winner '(alice) '(bob))                        ;; expression 1

;(equal? '(alice) (winner '(alice) '(bob)))      ;; expression 2

;(and (equal? '(alice) (winner '(alice) '(bob))) ;; expression 3
;     (equal? '(alice) (winner '(alice) '(fred))))

;(and (equal? '(alice) (winner '(alice) '(bob))) ;; expression 4
;     (equal? '(jane) (winner '(jane) '(fred))))

;a)
;Expression 1: there is a call for the procedure winner, that decides which team is
;winner. Winner procedure recalls total-pulling team procedure for each team (in this
;case Alice and Bob). It basically sum the strength of each person in the team.
;Strength of each person is memorized as 5 if flip is true, 10 otherwise. Then, strength
;depends on lazyness of a person. Lazyness is expressed as a unfair flip of a coin,
;where 1 times of 3 is true, false otherwise. If the person is lazy, then its strength
;is halved. Finally, the winner team is the one with the bigger total strength.
;Results are going to change at each run.
;The winner is Alice with probability 0.69, Bob with 0.31.

;Expression 2: the expression is similar to the first one, but there is another step of
;the process: using equal? we want to check if Alice is the winner between Alice and
;Bob. In this case, the expression will not return the name of the winner but #t if
;Alice is the winner, #f otherwise.
;If this expression is computed after the first one, alice's strength would be kept in
;memory. So, the probability ...... write

;Expression 3: in this case, we want to check if Alice both wins against Bob and Fred.
;If so, the expression would return #t; it would return false if Alice is defeated by
;one between Bob and Fred, or both. ..... reason about probability

;Expression 4: this is similar to the expression 3, but in this case we want to check
;if Alice wins against Bob and Jane wins against Fred. The expression returns #t if both
;win, #f if one of them lose agains their opponents. ..... write about probability

;b) The probability that a person has a particular strength is showed in the picture. For
;each expression, we must calculate the probability that the person wins.
;Expression 1: in this case, we want to know if Alice wins so we must sum the upper right
;corner of the matrix, because if both have the same strength, the first team wins as
;showed by the else clause. The probability is P(Alice=wins)= 0,69
;Expression 2: this expression is equal to the first one, except for the clause that
;compares the result of the winner procedure with alice. So the probability is the same as
;the first expression.
;Expression 3: need to calculate probability that wins if she already win against bob (see
;pic for probability)
;Expression 4: in order to get #t from the expression, we must calculate the probability
;that Alice and Jane wins against Bob and Fred. The probabilities are independent so we
;only need to multiply 0,69 and 0,68. Therefore, P(Alice=wins,Jane=wins)=0,4761.

;c)in the first one there is the same player, while in the second there are different players
;so probabilities are independent.

;6
(define (geometric p)
  (if (flip p)
      0
      (+ 1 (geometric p))))

;(hist (repeat (lambda () (geometric 0.5)) 300))

;Flip p will return true depending on the probability p, and false otherwise (1-p).
;In order to obtain 5, the geometric distribution should give #f, 5 times in a row
;and then a true.
;Each time, the result is independent from the previous one, so P(5)=(1-p)^5p.
;We can note that the probability decreases when p is higher (because there is 1-p)
;and increases when p is smaller.
;For example, for p = 0.5, the probability of getting 5 is (0.5^5)(0.5) = 0.015625.

;7
;(define (a) (flip 0.8))
;(define (b) (flip (if (a) 0.5 0.3)))
;(define (c) (list (a) (b)))

(define (c)
  (define a (flip 0.5))
  (define (b a) (flip (if a 0.2 0.03)))
  (list a (b a)))

;(hist (repeat c 1000))
;Probability of A being true is fixed to 0.8, and B depends on A (0.5 if A is true,
;0.3 otherwise).
;Hist will show that I get correct distribution.


;Exercise 4
; What are (bernoulli-dist p), (normal-dist μ σ) exactly? Are they real numbers (produced in a random way)?
; We have seen that flip is a procudere with a probabilistic behaviour. Is, e.g., (normal-dist μ σ) something similar?
; TRY TO EVALUATE (normal-dist 0 1)
; The evaluation just returns the distribution as output, in fact we defined a new object that is
; a distribution object. Flip, instead, is a sample of a probability distribution.
; (bernoulli-dist p), (normal-dist μ σ) are distribution objects and not real numbers.

; EXERCISE
; Evaluate
; (dist? (normal-dist 0 1))
; (dist? (bernoulli-dist 0.5))
; (dist? flip)
; The first two are a distribution, because dist? returns true. In fact, to check if
; something is a distribution-object, we use the procedure dist?.
; (dist? flip) returns false. This means that flip procedure is not a distribution,
; but it is a sample of a probability distribution (bernoulli distribution with
; probability 0.5).

; What is the difference between flip and (bernoulli-dist 0.5)?
; The procedure flip is basically (λ () (sample (bernoulli-dist 0.5))), with 0 instead
; of #f and 1 instead of #t.
; In fact, in order to produce probabilistic behaviours from a distribution, we use
; the procedure sample.


;Exercise 5
;a) It is explainable using the statement: given that Bob wins, which letter did he probably have?
;b) Using Bayes rule, P(h|win) is proportional to P(h)P(win|h).
;We can consider P(win) as the sum of P(h)P(win|h) for all the values of h, then divide
;P(h)P(win|h) by P(win) in order to obtain P(h|win).
;Here an Excel sheet (see pic).

;for code see prova.rkt

;c)my-list-index is a procedure that returns the first index (counter) of a
;list (haystack) whose element is equal to needle (?).
;if we ran
;(my-list-index 'mango '(apple banana) 1)
;the procedure would recursively search for an index starting from 1 of the list
;(apple banana) that is equal to mango. (show pic of procedure)
;It checks if the list is empty, if so returns an error. Otherwise, checks if the
;first element of the list is equal to the needle argument. If so, returns the counter;
;otherwise the procedure is evaluated again using the rest of the list (without the
;first element) and with counter + 1.
;In this case, it returns the error of the procedure: it means the it checked all the list
;with no success.

;d)See code on picture
;Multinomial procedure samples an element from a list of categories with the respective
;probability. It requires as input a list of categories and a list of probabilities.
;It returns an element of the category using its probability.
;For example, using 1000 repetition, the procedure got 46% of red (0.5 of probability), 42%
;of green (0.4 of probability), almost 6% of blue (0.05 of probability) and 5% of red (0.5 of
;probability). So the probability of getting a category are basically the ones given in input.

;e)The letter with highest posterior probability is B. In English, it means that, knowing that
;Bob won, B is the most likely letter he got.
;It easy to notice that the Church and the excel results are equal.

;f)It is possible to calculate the posterior probability of vowels and consonants adding an if
;clause to the query of the distribution using vowel? procedure. The barplot shows that vowels
;have an higher posterior probability than consonants.

;g)Calculating the probability in Church or Racket is very efficient because it is possible to
;model big problems using just a few lines of code. An alternative (which I personally used to
;manually calculate the probabilities) is Excel, but it would be a very long work if the problem
;does not have the appropriate size.
;In general anyway, Excel is more understandable for a common user than Church/Racket, but
;I personally prefer Church because of my programming experience.


;Exercise 6
;; EXERCISE. To see the problems of rejection sampling, consider the following
; variation of the previous example:

(define baserate 0.01)

(define (take-sample)
  (rejection-sampler
   (define A (if (flip baserate) 1 0))
   (define B (if (flip baserate) 1 0))
   (define C (if (flip baserate) 1 0))
   (define D (+ A B C))
   (observe/fail (>= D 2))
   A))

;(hist (repeat (take-sample) 100))

; Try to see what happens when you lower the basesate.
; What happens if we set it to 0.01?
; And to 0.001?

;The procedure take-sample uses rejection sampling to return samples that satisfy the
;condition of the observe/fail clause. If we decrease the baserate, the probability of
;satisfying the condition decreases, so the interpreter needs more time to find correct
;samples. This is one of the main problem of the rejection sampling.