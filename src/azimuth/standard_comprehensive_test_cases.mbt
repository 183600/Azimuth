// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "divide_with_ceil_basic" {
  // 测试基本的向上取整除法
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(4, divide_with_ceil(11, 3))
}

test "divide_with_ceil_negative" {
  // 测试负数的向上取整除法
  assert_eq(-2, divide_with_ceil(-7, 4))
  assert_eq(-3, divide_with_ceil(-10, 4))
  assert_eq(2, divide_with_ceil(7, -4))
}

test "divide_with_ceil_exact" {
  // 测试能整除的情况
  assert_eq(5, divide_with_ceil(10, 2))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(7, divide_with_ceil(14, 2))
}

test "add_edge_cases" {
  // 测试加法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  assert_eq(max_val, add(max_val, 100))
  assert_eq(max_val, add(max_val, max_val))
  
  // 测试最小值边界
  assert_eq(min_val, add(min_val, -100))
  assert_eq(min_val, add(min_val, min_val))
  
  // 测试零值
  assert_eq(0, add(0, 0))
  assert_eq(100, add(0, 100))
  assert_eq(-100, add(0, -100))
}

test "multiply_edge_cases" {
  // 测试乘法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试零值
  assert_eq(0, multiply(0, max_val))
  assert_eq(0, multiply(0, min_val))
  assert_eq(0, multiply(0, 0))
  
  // 测试单位值
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(max_val, multiply(-max_val, -1))
  
  // 测试溢出保护
  assert_eq(max_val, multiply(46341, 46341))
  assert_eq(min_val, multiply(min_val, -1))
}

test "greet_function_variations" {
  // 测试 greet 函数的各种情况
  // 测试空字符串
  assert_eq_string("Hello, !", greet(""))
  
  // 测试单字符
  assert_eq_string("Hello, A!", greet("A"))
  
  // 测试长字符串
  let long_name = "VeryLongNameThatTestsStringHandling"
  assert_eq_string("Hello, VeryLongNameThatTestsStringHandling!", greet(long_name))
  
  // 测试包含空格的字符串
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  
  // 测试特殊字符
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
}

test "mathematical_combinations" {
  // 测试数学运算的组合
  let a = 12
  let b = 8
  let c = 5
  
  // 测试 (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(100, result1)
  
  // 测试 a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(101, result2)
  
  // 测试 (a - b) * c
  let result3 = multiply(add(a, -b), c)
  assert_eq(20, result3)
}

test "real_world_calculations" {
  // 测试实际应用场景的计算
  // 计算矩形的面积和周长
  let length = 10
  let width = 6
  
  // 面积 = 长 × 宽
  let area = multiply(length, width)
  assert_eq(60, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  assert_eq(32, perimeter)
  
  // 计算平均值
  let scores = add(add(add(85, 90), 78), 92)
  let average = scores / 4
  assert_eq(86, average)
}

test "complex_business_logic" {
  // 测试复杂的业务逻辑
  // 订单计算：商品价格 × 数量 + 运费 - 折扣
  let item_price = 199
  let quantity = 3
  let shipping = 20
  let discount = 50
  
  let subtotal = multiply(item_price, quantity)
  let total = add(add(subtotal, shipping), -discount)
  assert_eq(627, total)
  
  // 利润计算：(收入 - 成本) / 成本 × 100%
  let revenue = 1500
  let cost = 1000
  let profit = multiply(add(revenue, -cost), 100) / cost
  assert_eq(50, profit)
}

test "algorithm_scenarios" {
  // 测试算法场景
  // 计算等差数列的和：1 + 2 + 3 + ... + 10
  let n = 10
  let sum = multiply(n, add(n, 1)) / 2
  assert_eq(55, sum)
  
  // 计算等比数列的和：2 + 4 + 8 + 16
  let geo_sum = add(add(add(2, 4), 8), 16)
  assert_eq(30, geo_sum)
  
  // 计算阶乘：5! = 5 × 4 × 3 × 2 × 1
  let factorial = multiply(5, multiply(4, multiply(3, multiply(2, 1))))
  assert_eq(120, factorial)
}

test "assert_function_comprehensive" {
  // 综合测试断言函数
  // 测试 assert_true 和 assert_false
  assert_true(10 > 5)
  assert_true(add(3, 4) == 7)
  assert_true(multiply(2, 6) == 12)
  
  assert_false(10 < 5)
  assert_false(add(3, 4) == 8)
  assert_false(multiply(2, 6) == 13)
  
  // 测试 assert_eq
  assert_eq(15, add(7, 8))
  assert_eq(0, add(5, -5))
  assert_eq(100, multiply(10, 10))
  
  // 测试 assert_eq_string
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, Test!", greet("Test"))
  assert_eq_string("Hello, 123!", greet("123"))
}