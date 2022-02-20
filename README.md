# Murphy
Simple Functional Programming Language ( Scanner, Parser, Interpreter )

## Libraries

No external libraries were used, you only need to have OCaml installed.

## Preview

![image](https://user-images.githubusercontent.com/69087218/154868581-168e9e68-7087-4d62-895c-cbe33c2e805b.png)


## Project setup (Linux)

- #### Compile the file murphy.ml

  ### `ocamlc murphy.ml -o murphy.byte`

- #### Write your code in a file name.txt and run it

  ### `./murphy.byte ./name.txt`
  
  
## Syntax

This language only works with Integer Numbers

### Algebraic expressions

```javascript
 (exp)      //brackets
 exp + exp  //sum
 exp - exp  //difference
 exp * exp  //multiplication
 exp / exp  //division
  ```
  
  examples
  
  ```javascript
   5 - 2      //3
   0/0        //NaN
   2/0        //PLUS_INFINITY
   5 - 1/0    //MINUS_INFINITY 
   2/0 - 3/0  //NaN
   ...
  ```
  
 ### Comparison

```javascript
 exp == exp
 exp < exp
 exp > exp
  ```
  
  examples
  
  ```javascript
   3 + 2 == 5   //TRUE
   1 == 0       //FALSE
   1 > 0        //TRUE
   1 < 0        //FALSE
   ...
  ```
  
  ### TEST statement

```javascript
 TEST exp [ == | < | > ] exp
 SUCCESS
    exp
 FAIL 
    exp
  ```
  
  examples
  
  ```javascript
   TEST (5 + 5) / 2 == 5
   SUCCES
      10
   FAIL
      1/0
   ...
  ```
  
  ### LET statement

```javascript
 LET ide = exp
 IN
    exp
  ```
  
  examples
  
  ```javascript
   LET x = 1
   IN
   
   LET y = 2
   IN
   
   x*x + y*y
   ...
  ```
  
### Function declaration

```javascript
 FUNC fname (ide1, ide2, ...) =
    exp
 IN
    exp
  ```
  
  examples
  
  ```javascript
   FUNC sum (x, y) = x + y
   IN
    5 + 5 
   ...
  ```
  
 ### Function call

```javascript
 fname (exp1, exp2, ...)
  ```
  
  examples
  
  ```javascript
   FUNC sum (x, y) = x + y
   IN
    5 + sum(2, 3) //10
   ...
  ```
 

  
