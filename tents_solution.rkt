#lang scheme
; 2019400258

(define ADJACENT (lambda(x y)

(if(null? x) #t (if(or (eq? (car x) (- (car y) 1))  (eq? (car x) (+ (car y) 1)) (eq? (car x) (car y))  )   ( ADJACENT (cdr x) (cdr y)   )    #f))  ))


(define NEIGHBOR-LIST (lambda (x )
   
(cons (list  (car x)  (+ (cadr x) 1))(cons (list (+ (car x) 1)  (cadr x)) (cons (list  (car x)   (- (cadr x) 1)) (list(list (- (car x) 1)  (cadr x))))))


                        ))

(define ADJACENT-WITH-LIST (lambda (x y)

(if (null? y)#f(if ( ADJACENT x (car y) ) #t (ADJACENT-WITH-LIST x (cdr y))))))

(define REPLACE-NTH
  (lambda (list1  nth item )
    (if (= nth 1) (cons item (cdr list1)) (cons (car list1) (REPLACE-NTH  (cdr list1) (- nth 1) item  )))))


                             
(define RETURN-FIRST-NOT-FALSE(lambda ( f l)

 ( if (null? l)
     #f
      
       (if (number? (f (car l)) )
           (f(car l))
            (RETURN-FIRST-NOT-FALSE f (cdr l))))))




    

       (define deleteItem1 (lambda(lst countx county x y tree )
       (if (null? lst)
         '()
        (if (or(eq? 0 (car(car lst)))  (eq? 0 (cadr(car lst))) (< countx (car(car lst)))  (< county (cadr(car lst)))  (eq? 0 (at (car(car lst)) x) )   (eq? 0 (at (cadr(car lst)) y) ) (listevar 1 (car lst) tree) )  
        (deleteItem1 (cdr lst ) countx county x y  tree )
       
         (cons (car lst) (deleteItem1 (cdr lst ) countx county x y  tree))))))


 

  
  (define TENTS-SOLUTION2 (lambda(list x y n countlist solution)


           (if (and (null? solution) (> countlist (howMany(findlimit list 1))))     
              #f

           (if(eq? (- n 1) ( howMany (caddr list)) )
              
            solution

            (if(null? (findlimit list n) )
               #f




            (if(> countlist ( howMany (findlimit list n)))
                   ( TENTS-SOLUTION2 list (increaseone (car (car solution) ) x) (increaseone (cadr (car solution) ) y) (- n 1) (+(kacıncı 1 (car solution ) (findlimit list (- n 1)) )1)  (cdr solution))
             



                (if (free x y (at countlist (findlimit list n)))                 

                (if(eq? countlist ( howMany (findlimit list n)) )
                  ( TENTS-SOLUTION2 list (increaseone (car (car solution) ) x) (increaseone (cadr (car solution) ) y) (- n 1) (+(kacıncı 1 (car solution ) (findlimit list (- n 1)) )1) (cdr solution))
                  ( TENTS-SOLUTION2 list x y n  (+ countlist 1)  solution))



            ( if (ADJACENT-WITH-LIST (at countlist(findlimit list  n))   solution )
                 (if(eq? countlist ( howMany (findlimit list n)) )
                  ( TENTS-SOLUTION2 list (increaseone (car (car solution) ) x) (increaseone (cadr (car solution) ) y) (- n 1) (+(kacıncı 1 (car solution ) (findlimit list (- n 1)) )1) (cdr solution))
                  ( TENTS-SOLUTION2 list x y n  (+ countlist 1)  solution))
                 (TENTS-SOLUTION2  list (decreaseone (car (at countlist (findlimit list n))) x)  (decreaseone (cadr (at countlist (findlimit list n))) y) (+ n 1) 1 ( cons (at countlist (findlimit list n)) solution)))
             )))))))
                  


                    
 
         
                
                 
   (define TENTS-SOLUTION (lambda(list)


                           (if(eq? (sum (car list) ) (howMany (caddr list))  )

                              (if(eq? (howMany (caddr list)) 0)
                                 '()



                              ( TENTS-SOLUTION2  list (car list) (cadr list) 1 1 '()))


                              #f)))


                         

                                                                             
(define free (lambda (x y list)

(if(or (eq? (at (car list) x) 0) (eq?(at (cadr list) y) 0)) 
  #t
  #f
)
               
                 ) )


               
                                                                                                                                                           

(define howMany (lambda( list)
  (if (null? list)
      0
      (+ 1 (howMany (cdr list))))
  )
)


(define at(lambda ( n list)
    (if (null? list)
        list
         (if (= n 1)
             (car list)
           (at (- n 1) (cdr list))))))


(define decreaseone (lambda(x ls)
  (if (null? ls)
      '()
      (if (= x 1)
          (cons (- (car ls) 1) (decreaseone (- x 1) (cdr ls)))
          (cons (car ls)(decreaseone (- x 1) (cdr ls)))))))
(define increaseone(lambda( x ls)
  (if (null? ls)
      '()
      (if (= x 1)
          (cons (+ (car ls) 1) (increaseone (- x 1) (cdr ls)))
          (cons (car ls)(increaseone (- x 1) (cdr ls)))))))



(define findlimit (lambda(list n)

                            (define x (car list))
                           (define countx (howMany x))
                           (define y (cadr list))
                           (define county (howMany y))
                           (define tree (caddr list))
                           
                           (define lst (NEIGHBOR-LIST (at n tree) ))
                        
                          
                              (deleteItem1 lst countx county x y tree)


                    ))

(define kacıncı (lambda (n k list)
    (if(and (eq? (car(at n list)) (car k))   (eq? (cadr(at n list)) (cadr k))   )
       n
       (kacıncı (+ n 1) k list))


                  
                    ))

(define listevar (lambda(n k list)


               (if(and (eq?(car k) (car(at n list)))   (eq?(cadr k) (cadr(at n list)))  )
                  #t



                  (if(eq? n (howMany list))
                   #f
                   (listevar (+ n 1) k list )))))

     (define sum (lambda( List)
  (if
    (null? List)
    0
    (+ (car List) (sum (cdr List)))
  )))


     
                    
                   


                   



                  




                
  