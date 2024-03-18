http://learnyouahaskell.com/functors-applicative-functors-and-monoids
https://quangchien.wordpress.com/2012/06/01/ch11-learn-haskell/#applicative-functors


# Functor #
fmap :: (Functor f) => (a -> b) -> f a -> f b
- fmap nhận một hàm a -> b rồi trả lại một hàm f a -> f b. Việc này được gọi là nâng hàm lên. 
- Bạn có thể hình dung fmap như là một hàm nhận một hàm khác và một functor rồi ánh xạ hàm khác đó lên functor, hoặc cũng có thể hình dung nó như một hàm nhận một hàm khác rồi nâng hàm đó lên để nó hoạt động được với các functor. 

### List ###
```
instance Functor [] where
    fmap = map
```
### Maybe ###
```
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing
```
### Either ###
```
instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x
```
### IO action ###
```
instance Functor IO where
    fmap f action = do
        result <- action
        return (f result)
```
### Function (->) ###
```
instance Functor ((->) r) where
    fmap f g = (\x -> f (g x))
  --fmap = (.)
```

## Các định luật functor: ##
Các định luật này không phải do Haskell kiểm soát, vì vậy bạn phải tự tay kiểm tra lấy.
```
fmap id = id 
fmap (f . g) = fmap f . fmap g
```
Tất cả những thực thể Functor trong thư viện chuẩn đều tuân theo các định luật này. Ta có thể dùng các định luật để suy diễn về các hành vi mà bất kì functor nào cũng phải có, và tạo ra các hàm hoạt động một cách đáng tin cậy trên bất kì functor nào.
Khi định nghĩa 1 kiểu dữ liệu muốn trở thành thực thể của Functor, cần kiểm tra kĩ xem nó có thỏa mãn 2 định luật này không.

Một cách khác để hình dung functor: có thể hình dung việc ánh xạ lên các functor cũng như gắn một phép biến đổi lên đầu ra của 1 functor khiến nó thay đổi giá trị, do đó trả về 1 functor khác.


# Applicative #
```
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```
`pure` nhận một giá trị rồi đặt nó vào trong ngữ cảnh tối thiểu mặc định miễn là đủ chứa giá trị đó.

instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> something = fmap f something
    
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]
Bạn có thể coi List như những đại lượng (hay đại lượng tính toán) không tất định (có nhiều lựa chọn). Vì vậy, chẳng hạn khi bạn viết (+) <$> [1,2,3] <*> [4,5,6], thì bạn có thể coi nó như là cộng hai đại lượng không tất định, để tạo ra một đại lượng không tất định khác mà thậm chí còn ít khả năng quyết định kết quả hơn nữa (có nhiều lựa chọn kết quả hơn do sự tổ hợp các lựa chọn của 2 đại lượng đầu vào).

instance Applicative IO where
    pure = return
    a <*> b = do
        f <- a
        x <- b
        return (f x)
        
instance Applicative ((->) r) where
    pure x = (\_ -> x)
    f <*> g = \x -> f x (g x)
Việc viết k <$> f <*> g tạo ra một hàm mà sẽ gọi k với các kết quả cuối cùng từ f và g    

instance Applicative ZipList where
    pure x = ZipList (repeat x)
    ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)
    

liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])

sequenceA [Just 3, Just 2, Just 1]
> Just [3,2,1]

sequenceA [(>4),(<10),odd] 7
> [True,True,True]

sequenceA [[1,2,3],[4,5,6]]
> [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]

Các định luật :
```
	pure f <*> x = fmap f x
	pure id <*> v = v
	pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
	pure f <*> pure x = pure (f x)
	u <*> pure y = pure ($ y) <*> u
```

#Kiểu mới với newtype
Dùng từ khóa newtype giống như bocj một kiểu có sẵn lại để trở thành một kiểu mới
'newtype' có tính lười biếng hơn 'data'
VD:
data CoolBool = CoolBool { getCoolBool :: Bool }
helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

ghci> helloMe undefined
"*** Exception: Prelude.undefined

Nếu CoolBool được định nghĩa bằng newtype thì không có lỗi xảy ra
newtype CoolBool = CoolBool { getCoolBool :: Bool }

ghci> helloMe undefined
"hello"

- Phân biệt trường hợp sử dụng type, newtype, data
+ type dùng để tạo ra kiểu mới hoàn toàn tương đương với kiểu cũ, Haskell coi 2 kiểu cũ, mới này là như nhau, dùng thay thế cho nhau, hàm nào dùng được với kiểu cũ thì cũng dùng được với kiểu mới -> Dùng khi cần thay thế tên kiểu cũ bằng một tên đặc tả, dễ nhớ, phù hợp với mục đích sử dụng
+ Giống như type, newtype tạo ra kiểu dữ liệu mới xuất phát từ 1 kiểu cũ, nhưng Haskell coi 2 kiểu này là khác nhau, kiểu mới không kế thừa những thể hiện lớp của kiểu cũ, người viết cần tự định nghĩa các instance (vẫn có thể sử dụng deriving) tùy theo mục đích sử dụng. -> Dùng khi muốn tạo ra kiểu mới xuất phát từ kiểu cũ nhưng muốn thay đổi 1 số hành vi thể hiện lớp của nó. Khi khai báo newtype dạng record, ta có 2 hàm để chuyển đổi dữ liệu qua lại giữa 2 kiểu là Value Contructor và tên trường record.
+ data dùng khi muốn tạo ra 1 kiểu mới hoàn toàn: có nhiều value contructor, nhiều trường record 

# Monoid
Monoid xuất hiện khi bạn có một hàm nhị phân, có tính kết hợp, và một giá trị đóng vai trò như một “phần tử trung hoà” của hàm.

Các định luật Monoid:
mempty `mappend` x = x
x `mappend` mempty = x
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

instance Monoid [a] where
    mempty = []
    mappend = (++)
    
Một số thực thể Monoid khác: Sum, Product, Any, All, Ordering, Maybe

instance Monoid Ordering where
    mempty = EQ
    LT `mappend` _ = LT
    EQ `mappend` y = y
    GT `mappend` _ = GT
VD:
lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (x `compare` y)

instance Monoid a => Monoid (Maybe a) where
    mempty = Nothing
    Nothing `mappend` m = m
    m `mappend` Nothing = m
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)
    
newtype First a = First { getFirst :: Maybe a }
    deriving (Eq, Ord, Read, Show)
instance Monoid (First a) where
    mempty = First Nothing
    First (Just x) `mappend` _ = First (Just x)
    First Nothing `mappend` x = x

First có ích khi ta có một loạt những giá trị Maybe và muốn biết xem trong số chúng có cái nào là Just không. Hàm mconcat trở nên có ích:
ghci> getFirst . mconcat . map First $ [Nothing, Just 9, Just 10]
Just 9

ghci> getLast . mconcat . map Last $ [Nothing, Just 9, Just 10, Nothing]
Just 10

#Dùng Monoid với Foldable
Class Foldable đại diện cho các cấu trúc dữ liệu có thể gấp (duyệt): [], Maybe, Either ...
Muốn kiểu Tree thành instance của Foldable cần triển khai foldMap hoặc foldr. Dùng Monoid để triển khai như sau
```
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
instance Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                             f x           `mappend`
                             F.foldMap f r

testTree = Node 5
            (Node 3
                (Node 1 Empty Empty)
                (Node 4 Empty Empty)
            )
            (Node 9
                (Node 8 Empty Empty)
                (Node 10 Empty Empty)
            )
            
ghci> getAny $ F.foldMap (\x -> Any $ x == 3) testTree
True

ghci> getAny $ F.foldMap (\x -> Any $ x > 15) testTree
False

ghci> F.foldMap (\x -> [x]) testTree
[1,3,4,5,8,9,10]
```
