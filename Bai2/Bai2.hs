-- Question 1
-- Add the type signatures for the functions below and then remove the comments and try to compile.
-- (Use the types presented in the lecture.)
f1 :: Double -> Double -> Double -> Double
f1 x y z = x ** (y / z)

f2 :: Double -> Double -> Double -> Double
f2 x y z = sqrt (x / y - z)

f3 :: Bool -> Bool -> [Bool]
f3 x y = [x == True] ++ [y]

f4 :: [Int] -> [Int] -> [Int] -> Bool
f4 x y z = x == (y ++ z)

-- Question 2
-- Why should we define type signatures of functions? How can they help you? How can they help others?

-- Type Signatures giúp giới hạn phạm vi kiểu dữ liệu vào ra của hàm theo ý người viết. Nếu không khai báo, trình biên dịch sẽ tự suy luận kiểu, có thể khác so với mong đợi, dễ dẫn đến lỗi không mong muốn sau này. Lỗi đó có thể ở rất xa điểm phát sinh, mất nhiều thời gian để phát hiện.
-- Ngoài ra, Type Signatures giúp code dễ đọc, dễ hiểu hơn.

-- Question 3
-- Why should you define type signatures for variables? How can they help you?
-- Cũng giống như đối với hàm, nên khai báo kiểu cho biến, nhất là đối với người mới, để tránh các lỗi phát sinh. Người mới chưa quen đọc Error message, khó tìm ra lỗi.


-- Question 4
-- Are there any functions in Haskell that let you transform one type to the other? Try googling for the answer.

--read : Đọc 1 chuỗi và trả về 1 giá trị kiểu khác (nên chỉ định kiểu trả về)
x1 :: Int
x1 = read "12"
x2 = read "8.2" :: Double
x3 = read "True" && False

--fromInteger :: Đổi từ số Integer sang kiểu số khác
y1 = fromInteger 2 :: Float
y2 = (fromInteger 45) / 3.5

-- Question 5
-- Can you also define in Haskell list of lists? Did we showed any example of that? How would you access the inner
-- most elements?
listOfThreeLists :: [[Int]]
listOfThreeLists = [[0, 1, 2, 3], [4, 5, 6], []]

-- Access inner elements use (!!)
item_1_3 = listOfThreeLists !! 1 !! 2
item_0_1 = head listOfThreeLists !! 1