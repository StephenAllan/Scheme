;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Project2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

#||
  Project 2: Scheme
  
  This class contains basic implementations of select Scheme math and list features.

  @author  Stephen Allan (swa9846)
  @version September 24th, 2016
 ||#



#| REQUIRED FUNCTIONS |#



#|
  Calculate the modulus of two numbers in the form of 'dividend mod divisor'.
  A modulus is the remainder value which results from a division operation.

  @param dividend Number to be divided
  @param divisor  Number to divide by
  @return  Remainder after the dividend is divided by the divisor
 |#
(define (myMod dividend divisor)
  (cond
    ((= dividend divisor) 0)
    ((< dividend divisor) dividend)
    ((> dividend divisor) (myMod (- dividend divisor) divisor))
  )
)


#|
  Calculates the square root of the input integer value.

  @param x Number to square root
  @return  Square root of the input integer value
 |#
(define (mySqrt x)
  (if (= x 0)
    x
    (recSqrt x 1 0)
  )
)


#|
  Calculates the cube root of the input integer value.
     
  @param x Number to cube root
  @return  Cube root of the input integer value
 |#
(define (myCbrt x)
  (if (= x 0)
    x
    (cond
      ((< x 0) (* (recCbrt (* x -1) 1 0) -1))
      ((> x 0) (recCbrt x 1 0))
    )
  )
)


#|
  Calculates the greatest common divisor between two integers.
  The gcd is the largest number which can divide both input values.
  
  @param x  First number to calculate gcd for
  @param y  Second number to calculate gcd for
  @return  Largest number which can divide both input values
 |#
(define (myGcd x y)
  (if (= y 0)
    x
    (myGcd y (myMod x y))
  )
)


#|
  Calculates the least common multiple of the two given integer values.
  
  @param x  First number to calculate lcm for
  @param y  Second number to calculate lcm for
  @return  Lowest number which is a multiple of both input values
 |#
(define (myLcm x y)
  (cond
    ((= x 0) 0)
    ((= y 0) 0)
    (else (recLcm x y x y))
  )
)


#|
  Sorts the given interger list in ascending order.
  Returns a copy of the given array, the original is never modified.
  
  @param lst  Input list to sort
  @return  Sorted copy of the original list
 |#
(define (mySort lst)
  (if (null? lst)
    `()
    (insertionSort (car lst) (mySort (cdr lst)))
  )
)


#|
  Sorts the input list in decending order, returning the head of the list as the largest element.

  @param lst  Input list from which to select the largest value
  @return  Largest value within the input list
 |#
(define (myMax lst)
  (car (reverse (mySort lst)))
)


#|
  Sorts the input list in ascending order, returning the head of the list as the smallest element.
  
  @param lst  Input list from which to select the lowest value
  @return  Lowest value within the input list
 |#
(define (myMin lst)
  (car (mySort lst))
)


#|
  Search for a given integer value within a given integer input list.
  
  @param lst   Input list in which to search for the given value
  @param value Number to find in the input list
  @return  true if the value is found in the input list, false otherwise
 |#
(define (inList lst value)
  (if (null? lst)
    0
    (if (= (car lst) value)
      1
      (inList (cdr lst) value)
    )
  )
)


#|
  Calculates the average of all of the values within an interger list.
  
  @param lst  Input list from which to find the average
  @return  Average of the values within the input list
 |#
(define (avgList lst)
  (let ((avg (/ (sumList lst 0) (length lst))))
    (if (< avg 0)
      (ceiling avg)
      (floor avg)
    )
  )
)


#|
  Calculates the median number from a given list of integers.
  
  @param lst  Input list from which to find the median
  @return  Median number of the input list
 |#
(define (medList lst)
  (let
    ((mid (floor (/ (length lst) 2)))
     (sorted (mySort lst)))
    (if (= (myMod (length lst) 2) 0)
      (floor (/ (+ (getMiddle sorted mid) (getMiddle sorted (- mid 1))) 2))
      (getMiddle sorted mid)
    )
  )
)



#| HELPER FUNCTIONS |#



#|
  Calculates the square root of the input integer value.

  @param x     Number to square root
  @param index Current number to check if square root candidate
  @param sqrt  Current calculated square root
  @return  Square root of the input integer value
 |#
(define (recSqrt x index sqrt)
  (let ((square (* index index)))
    (if (<= square x)
      (recSqrt x (+ index 1) index)
      sqrt
    )
  )
)


#|
  Calculates the cube root of the input integer value.
     
  @param x     Number to cube root
  @param index Current number to check if cube root candidate
  @param cbrt  Current calculated cube root
  @return  Cube root of the input integer value
 |#
(define (recCbrt x index cbrt)
  (let ((cube (* (* index index) index)))
    (if (<= cube x)
      (recCbrt x (+ index 1) index)
      cbrt
    )
  )
)


#|
  Calculates the least common multiple of the two given integer values.

  @param x    First number to calculate lcm for
  @param y    Second number to calculate lcm for
  @param incX Size by which to increment parameter x
  @param incY Size by which to increment parameter y
  @return  Lowest number which is a multiple of both input values
 |#
(define (recLcm x y incX incY)
  (if (= x y)
    x
    (if (< x y)
      (recLcm (+ x incX) y incX incY)
      (recLcm x (+ y incY) incX incY)
    )
  )
)


#|
  Sorts value according to the elements in the input list lst.

  @param value Element to sort in the list
  @param lst   Input list in which to sort value
  @return  Copy of the input list with value sorted in that list
 |#
(define (insertionSort value lst)
  (if (null? lst)
    (list value)
    (if (< value (car lst))
      (cons value lst)
      (cons (car lst) (insertionSort value (cdr lst)))
    )
  )
)


#|
  Calculates the sum of the values within a given list.

  @param lst   Input list from which to calculate the sum
  @param value Current sum of the input values
  @return  Sum of the input values in lst
 |#
(define (sumList lst value)
  (if (null? lst)
    value
    (sumList (cdr lst) (+ (car lst) value))
  )
)


#|
  Finds the middle element from a given list.
  
  @param lst  Input list from which to find the middle index
  @return  Middle element of the input list
 |#
(define (getMiddle lst midCount)
  (if (= midCount 0)
    (car lst)
    (getMiddle (cdr lst) (- midCount 1))
  )
)