[Nguồn1](http://learnyouahaskell.com/a-fistful-of-monads)
[Nguồn2](http://learnyouahaskell.com/for-a-few-monads-more)

Functor đại diện cho các kiểu dữ liệu có thể ánh xạ một hàm lên nó
`fmap :: (Functor f) => (a -> b) -> f a -> f b`
`(<$>)` là dạng trung tố của `fmap`

Nếu hàm (a -> b) cũng được bọc trong 1 functor thì sao, khi đó Applicative xuất hiện, nó là lớp con của Functor:
`(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b`

Tiếp tục thay đổi một chút đối với hàm, nếu nó nhận vào 1 giá trị thông thường và trả về 1 Applicative -> Chúng ta có lớp con của Applicative là Monad:
`(>>=) :: (Monad m) => m a -> (a -> m b) -> m b`

Khi lần đầu tiên nói về functor, chúng ta đã thấy rằng đó là khái niệm hữu ích cho những giá trị mà ta có thể ánh xạ lên chúng. 
Sau đó, ta đã phát triển thêm khái niệm này bằng cách giới thiệu các functor áp dụng, vốn cho phép ta xem các giá trị thuộc kiểu dữ liệu nhất định như những giá trị gắn với ngữ cảnh và dùng các hàm thông thường lên các giá trị đó trong khi vẫn giữ được ý nghĩa của các ngữ cảnh đó.

Monad vốn là các functor ứng dụng được nâng cao, cũng giống như bản thân các functor ứng dụng là các functor nâng cao.

```
type Monad :: (* -> *) -> Constraint
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b   -- "bind"
  
  (>>) :: m a -> m b -> m b           -- "then"
  x >> y = x >>= \_ -> y
  
  return :: a -> m a
  
  {-# MINIMAL (>>=) #-}
```

`return` giống như `pure` nhận một giá trị rồi đặt vào trong ngữ cảnh tối thiểu mặc định, miễn là đủ chứa giá trị đó.

Ngoài ra, còn hàm flip bind là (=<<), ta có thể chứng minh được tính chất sau để thấy được sự liên quan của Monad và Functor:
    `(=<<) = (join .) . fmap`
Thay đổi 1 chút chữ kí kiểu của `fmap`, ta sẽ thấy rõ hơn sự liên quan
```
fmap  :: (Functor f) => (a -> c)    ->  f a  -> f c
fmap  ::             => (a -> m b)  ->  m a  -> m (m b)
join  :: (Monad m)   =>                         m (m b) -> m b
(=<<) :: (Monad m)   => (a -> m b)  ->  m a             -> m b
```
## MAYBE
```
instance Monad Maybe where
    return x = Just x
    
    Nothing >>= f = Nothing
    Just x >>= f  = f x
    
    fail _ = Nothing
```
Maybe là một monad với ngữ cảnh là tính toán có khả năng thất bại

-- VD Lần lượt cho chim đậu trên cây sào thăng bằng 
```
ghci> return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)
Nothing
```
Trong chuỗi tính toán, nếu xuất hiện 1 phép tính lỗi thì phép tính đó trả về `Nothing`, khiến toàn bộ các phép tính sau trong chuỗi cũng trả về `Nothing`. Và kết quả của toàn bộ chuỗi tính toán là `Nothing` 
Do đó, `maybe` được sử dụng để bắt lỗi tiềm ẩn của 1 loạt các hoạt động tính toán, mà không gây đổ vỡ chương trình

```
justH :: Maybe Char
justH = do
    (x:xs) <- Just "hello"
    return x
```
*Khớp mẫu* trong khối `do` nếu xảy ra lỗi thì hàm `fail` sẽ được gọi, trong trường hợp của Maybe, `fail` trả về `Nothing`
- Khớp mẫu với hàm thuần túy thông thường sẽ khớp lần lượt từng mẫu xem thỏa mãn không, nếu tất cả các mẫu đều không khớp -> lỗi, chương trình đổ vỡ
- Khớp mẫu với `let` -> lỗi ngay nếu mẫu không khớp

## LIST
```
instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)
    fail _ = []
```

Dạng gộp danh sách chỉ là cách viết tiện lợi cho việc dùng danh sách như những monad.
```
ghci> [ (n,ch) | n <- [1,2], ch <- ['a','b'] ]
[(1,'a'),(1,'b'),(2,'a'),(2,'b')]

ghci> [ x | x <- [1..50], '7' `elem` show x ]
[7,17,27,37,47]
```
Ta sẽ tìm cách để viết lại biểu thức trên theo cách Monad, với hàm `guard`

### MonadPlus, guard
Lớp MonadPlus là dành cho những monad nào có thể đóng vai trò như monoid
```
class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a

instance MonadPlus [] where
    mzero = []
    mplus = (++)

guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

ghci> guard (5 > 2) :: Maybe ()
Just ()
ghci> guard (1 > 2) :: Maybe ()
Nothing
ghci> guard (5 > 2) :: [()]
[()]
ghci> guard (1 > 2) :: [()]
[]

ghci> [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)
[7,17,27,37,47]

sevensOnly :: [Int]
sevensOnly = do
    x <- [1..50]
    guard ('7' `elem` show x)
    return x
```
Nếu `guard` được thực hiện thành công thì kết quả chứa trong nó là `()`. Vì vậy khi này ta dùng `>>` để phớt lờ `()` đó và biểu diễn kết quả là thứ gì đó khác theo sau.
Về cơ bản, một `guard` nói rằng: nếu giá trị boole này là `False` thì hãy tạo ra một thất bại ngay ở đây, còn không thì hãy tạo ra một kết quả tượng trưng là `()` bên trong nó.

VD: Tính vị trí có thể đi của quân mã sau 3 nước
```
type KnightPos = (Int,Int)
moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
               ]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c',r')

in3 :: KnightPos -> [KnightPos]
in3 start = do 
    first <- moveKnight start
    second <- moveKnight first
    moveKnight second
```

## Các định luật đối với monad
- Đơn vị trái:    `return x >>= f`    ==  `f x`
- Đơn vị phải:    `m >>= return`      ==  `m`
- Tính kết hợp:  `(m >>= f) >>= g`   ==  `m >>= (\x -> f x >>= g)`

## KIỂU WRITER
Thuộc module `Control.Monad.Trans.Writer` 
`Writer` dành cho những giá trị có giá trị khác gắn vào và đóng vai trò như một dạng để ghi chép. Writer cho phép ta thực hiện tính toán trong khi vẫn yên tâm rằng các giá trị ghi chép lại đã được kết hợp làm một và giá trị này sau đó được gắn vào với kết quả.
```
newtype Writer w a = Writer { runWriter :: (a, w) }
instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')
```
Thực thể `Writer` triển khai `fail` riêng, vì vậy nếu có mẫu cần khớp bị thất bại trong khối lệnh `do`, thì `error` sẽ được gọi đến.
```
logNumber :: Int -> Writer [String] Int
logNumber x = Writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"]
    return (a*b)
    
ghci> runWriter multWithLog
(15,["Got number: 3","Got number: 5","Gonna multiply these two"])
```

Tính ước chung nhỏ nhất
```
gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

ghci> mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)
8 mod 3 = 2
3 mod 2 = 1
2 mod 1 = 0
Finished with 1
```
Hàm `tell` nhận một giá trị monoid, như `["This is going on"]` và tạo ra một giá trị `Writer` để biểu diễn cho giá trị đại diện `()` đóng vai trò kết quả, nhưng lại có giá trị monoid gắn kèm theo. 
Việc nối dang sách các ghi chép cần chú ý để đưa về dạng `a ++ (b ++ (c ++ (d ++ (e ++ f))))` để đảm bảo tốc độ tối ưu của `++`, thay vì dạng `((((a ++ b) ++ c) ++ d) ++ e) ++ f`
Ở VD trên, danh sách được bổ sung theo dạng 1. VD về dạng 2:
```
gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        result <- gcdReverse b (a `mod` b)
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        return result
```

Để tránh việc xây dựng sai cách bổ sung danh sách, sử dụng kiểu DiffList
```
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)         --- DiffList id
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))  --- DiffList (f . g)

gcd' :: Int -> Int -> Writer (DiffList String) Int
gcd' a b
    | b == 0 = do
        tell (toDiffList ["Finished with " ++ show a])
        return a
    | otherwise = do
        result <- gcd' b (a `mod` b)
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
        return result
```
Kiểm tra hiệu năng
```
count :: Int -> Writer (DiffList String) ()
count 0 = do
    tell (toDiffList ["0"])
count x = do
    count (x-1)
    tell (toDiffList [show x])

count' :: Int -> Writer [String] ()
count' 0 = do
    tell ["0"]
count' x = do
    count' (x-1)
    tell [show x]

countDown :: Int -> Writer [String] ()
countDown 0 = do
    tell ["0"]
countDown x = do
    tell [show x]
    countDown (x-1)
    
ghci> mapM_ putStrLn . fromDiffList . snd . runWriter $ count 500000
ghci> mapM_ putStrLn . snd . runWriter $ count' 500000
ghci> mapM_ putStrLn . snd . runWriter $ countDown 50000
```
`count` và `countDown` có tốc độ tốt hơn rất nhiều so với `count'`

## READER - HÀM (->) r
Ngoài là Functor, Applicative, `(->) r` cũng là một Monad
```
instance Monad ((->) r) where
    return x = \_ -> x
    h >>= f = \w -> f (h w) w
```
```
addStuff :: Int -> Int
addStuff = do
    a <- (*2)
    b <- (+10)
    return (a+b)

addStuff' = (+) <$> (*2) <*> (+10)`

ghci> addStuff 3
19
ghci> addStuff' 3
19
```
`addStuff` tương đương với `addStuff'`
Cũng như các giá trị monad khác mà ta đã bắt gặp, một hàm cũng có thể được coi là giá trị kèm theo ngữ cảnh. Ngữ cảnh của hàm thể hiện ở chỗ giá trị đó vẫn chưa xuất hiện và ta phải áp dụng hàm này đối với thứ gì đó để thu được giá trị kết quả.
Trong trường hợp của `addStuff`, cả hai `(*2)` và `(+10)` đều được áp dụng đối với số 3. return (a+b) cũng làm vậy, nhưng nó phớt lờ đi và luôn biểu diễn kết quả là a+b. Bởi thế, monad hàm còn được gọi là monad đọc (reader). Tất cả các hàm loại này đều đọc từ một nguồn dữ liệu chung.
Như vậy, nếu ta có nhiều hàm cùng khuyết mất một tham số và cùng phải được áp dụng với một giá trị, thì ta có thể dùng monad đọc để kết xuất các giá trị tương lai từ những hàm này và cách tạo lập của >>= sẽ đảm bảo rằng mọi việc được thực hiện đúng.

**Triển khai Functor, Applicative, Monad đối với hàm:**
```
(<$>) f g x = f (g x)
(<*>) f g x = f x (g x)
(=<<) f G x = f (g x) x  -- (=<<) là hàm flip bind, đảo tham số của bind (>>=)
```

## MONAD STATE
`newtype State s a = State { runState :: s -> (a,s) }`
Trong thư viện `Control.Monad.State` mới, định nghĩa này đã được thay đổi (`type State s = StateT s Identity`). Có 1 hàm `state` làm thay nhiệm vụ cho value contructor `State` bên trên khi khai báo gíá trị state monad.
Định nghĩa trên vẫn xuất hiện trong thư viện `Data.Sequence.Internal`. 

```
instance Monad (State s) where
    return x = State $ \s -> (x,s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s
                                        (State g) = f a
                                    in  g newState
```
Vd: pop và push dữ liệu từ Stack là danh sách các số Int
```
type Stack = [Int]

pop :: State Stack Int
pop = State $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = State $ \xs -> ((),a:xs)

stackManip :: State Stack Int
stackManip = do
    push 3
    a <- pop
    pop
    
stackStuff :: State Stack ()
stackStuff = do
    a <- pop
    if a == 5
        then push 5
        else do
            push 3
            push 8

moreStack :: State Stack ()
moreStack = do
    a <- stackManip
    when (a == 100) stackStuff
```

### Lớp MonadState
Lớp này có hai hàm hữu ích là `get` và `put`. 

Đối với State, hàm `get` được thiết lập như sau:
`get = State $ \s -> (s,s)`
Hàm này lấy trạng thái hiện thời rồi đặt nó vào kết quả.

`put newState = State $ \_ -> ((),newState)`
`put` nhận một trạng thái mới rồi lập một hàm trạng thái để thay thế trạng thái hiện thời

VD:
```
stackyStack :: State Stack ()
stackyStack = do
    stackNow <- get
    if stackNow == [1,2,3]
        then put [8,3,1]
        else put [9,2,1]

ghci> runState stackyStack [1,2,3]
((),[8,3,1])
ghci> runState stackyStack [1,2]
((),[9,2,1])
```

### Randomness and the state monad
#### RANDOM
`System.Random`: Thư viện giải quyết nhiệm vụ chung là tạo số giả ngẫu nhiên (pseudo-random). Khái niệm "ngẫu nhiên" trong thư viện thực chất là "giả ngẫu nhiên" 

`Random` typeclass: dành cho những thứ có thể nhận giá trị ngẫu nhiên.

`RandomGen` typeclass: dành cho những kiểu có thể đóng vai trò nguồn phát sinh ngẫu nhiên

`StdGen` type: là instance tiêu chuẩn của lớp RandomGen, cung cấp 1 bộ số, đóng vai trò là nguồn tạo ngẫu nhiên

`mkStdGen :: Int -> StdGen`
`mkStdGen` nhận một số nguyên rồi dựa vào đó, trả lại một bộ tạo ngẫu nhiên.

`random :: (RandomGen g, Random a) => g -> (a, g)`
`random` nhận vào 1 bộ tạo ngẫu nhiên rồi trả về một giá trị ngẫu nhiên và 1 bộ tạo ngẫu nhiên mới


```
ghci> random (mkStdGen 100) :: (Int, StdGen)  
(-1352021624,651872571 1655838864) 
```
Chạy nhiều lần lệnh trên đều trả ra cùng 1 kết quả. Do đó `random` luôn trả về kèm theo 1 bộ tạo ngẫu nhiêu, dùng làm nguyên liệu cho các lần chạy sau

`getStdGen :: MonadIO m => m StdGen`
`getStdGen` gets the global pseudo-random number generator. Extracts the contents of globalStdGen. Khi chương trình bắt đầu, nó yêu cầu hệ thống cung cấp một bộ sinh số ngẫu nhiên tốt rồi lưu nó vào một thứ gọi là bộ phát sinh ngẫu nhiên tổng thể (global random generator). `getStdGen` sẽ đi lấy giúp bạn bộ sinh ngẫu nhiên tổng thể đó khi bạn gắn nó với thứ gì đó.

VD tung 3 đồng xu
```
threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen = 
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in  (firstCoin, secondCoin, thirdCoin)
```
Có thể dễ dàng chuyển sang sử dụng State monad:
```
randomSt :: (RandomGen g, Random a) => State g a
randomSt = State random

threeCoins :: State StdGen (Bool,Bool,Bool)
threeCoins = do
    a <- randomSt
    b <- randomSt
    c <- randomSt
    return (a,b,c)
```
State monad giúp xử lý nhanh gọn các trạng thái trung gian, thay cho việc chúng ta phải tự mình lưu lại các bộ tạo ngẫu nhiên trước mỗi lần gọi `random` tiếp theo.

## Either e a
`Either` gần như là một dạng `Maybe` cải tiến, để phục vụ tốt hơn trong việc ghi nhận các loại lỗi có thể xảy ra trong chương trình.
```
instance (Error e) => Monad (Either e) where
    return x = Right x 
    Right x >>= f = f x
    Left err >>= f = Left err
    fail msg = Left (strMsg msg)
```
So với `Maybe`, instance `Monad` của `Either e` có một yêu cầu phụ thêm, đó là kiểu của giá trị chứa trong `Left`, cái mà được đại diện bởi tham số kiểu `e`, phải là một thực thể của lớp `Error`. Lớp `Error` được dành cho những kiểu mà các giá trị có thể đóng vai trò thông báo lỗi. Lớp này định nghĩa hàm `strMsg`, vốn nhận một lỗi dưới dạng chuỗi rồi trả lại một giá trị như vậy.

## Một số hàm Monad
### liftM
`liftM :: (Monad m) => (a -> b) -> m a -> m b`
Nếu các thực thể Functor và Monad của một kiểu dữ liệu đều tuân theo các định luật functor và monad thì `liftM`, `fmap` khi hoạt động với kiểu đó là tương đương nhau.
Đây là cách mà `liftM` đã được tạo lập:
```
liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f m = m >>= (\x -> return (f x))
```
Hoặc với khối lệnh do:
```
liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f m = do
    x <- m
    return (f x)
```

### Các hàm monad tương đương trong Applicative
#### `ap` tương đương <*>
```
ap :: (Monad m) => m (a -> b) -> m a -> m b
ap mf m = do
    f <- mf
    x <- m
    return (f x)
```

#### liftM2 tương đương liftA2
```
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c  
liftA2 f x y = f <$> x <*> y  

liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM2 f m1 m2 = do { x1 <- m1; x2 <- m2; return (f x1 x2) }
```

#### Ngoài ra còn có liftM3, liftM4, liftM5

### join
```
join :: (Monad m) => m (m a) -> m a
join mm = do
    m <- mm
    m
```

*chưa đọc hết*
