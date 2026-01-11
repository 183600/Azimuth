// 标准MoonBit测试用例

// 测试用例1: 基本加法功能
///|
test "basic_addition_functionality" {
  // 测试正数加法
  assert_eq(5, add(2, 3))
  assert_eq(100, add(50, 50))
  assert_eq(0, add(0, 0))
  
  // 测试负数加法
  assert_eq(-5, add(-2, -3))
  assert_eq(-100, add(-50, -50))
  
  // 测试正负数混合加法
  assert_eq(0, add(5, -5))
  assert_eq(10, add(15, -5))
  assert_eq(-10, add(-15, 5))
}

// 测试用例2: 基本乘法功能
///|
test "basic_multiplication_functionality" {
  // 测试正数乘法
  assert_eq(6, multiply(2, 3))
  assert_eq(100, multiply(10, 10))
  assert_eq(0, multiply(5, 0))
  
  // 测试负数乘法
  assert_eq(6, multiply(-2, -3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(-6, multiply(2, -3))
  
  // 测试单位元素
  assert_eq(5, multiply(5, 1))
  assert_eq(5, multiply(1, 5))
  assert_eq(-5, multiply(-5, 1))
  assert_eq(-5, multiply(1, -5))
}

// 测试用例3: 字符串处理功能
///|
test "string_processing_functionality" {
  // 测试基本字符串拼接
  assert_eq("Hello, World!", greet("World"))
  assert_eq("Hello, MoonBit!", greet("MoonBit"))
  
  // 测试空字符串
  assert_eq("Hello, !", greet(""))
  
  // 测试单字符
  assert_eq("Hello, A!", greet("A"))
  
  // 测试数字字符串
  assert_eq("Hello, 123!", greet("123"))
  
  // 测试特殊字符
  assert_eq("Hello, @#$!", greet("@#$"))
}

// 测试用例4: 边界值测试
///|
test "boundary_value_tests" {
  // 测试Int32最大值
  let max_val = 2147483647
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  
  // 测试Int32最小值
  let min_val = -2147483648
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
  
  // 测试接近边界的值
  assert_eq(2147483646, add(2147483645, 1))
  assert_eq(-2147483647, add(-2147483648, 1))
}

// 测试用例5: 溢出处理测试
///|
test "overflow_handling_tests" {
  // 测试正数加法溢出
  assert_eq(2147483647, add(2147483647, 1))
  assert_eq(2147483647, add(2147483646, 2))
  assert_eq(2147483647, add(1000000000, 2000000000))
  
  // 测试负数加法溢出
  assert_eq(-2147483648, add(-2147483648, -1))
  assert_eq(-2147483648, add(-2147483647, -2))
  assert_eq(-2147483648, add(-1000000000, -2000000000))
  
  // 测试乘法溢出
  assert_eq(2147483647, multiply(46342, 46342))
  assert_eq(2147483647, multiply(100000, 30000))
  assert_eq(-2147483648, multiply(-46342, 46342))
}

// 测试用例6: 负数运算测试
///|
test "negative_number_operations" {
  // 测试负数加法
  assert_eq(-10, add(-5, -5))
  assert_eq(-15, add(-10, -5))
  assert_eq(0, add(-5, 5))
  
  // 测试负数乘法
  assert_eq(25, multiply(-5, -5))
  assert_eq(-25, multiply(-5, 5))
  assert_eq(-25, multiply(5, -5))
  
  // 测试最小值的特殊处理
  assert_eq(-2147483648, multiply(-2147483648, 1))
  assert_eq(-2147483648, multiply(-2147483648, -1))
}

// 测试用例7: 零值处理测试
///|
test "zero_value_handling" {
  // 测试加法中的零值
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(-5, add(-5, 0))
  assert_eq(-5, add(0, -5))
  assert_eq(0, add(0, 0))
  
  // 测试乘法中的零值
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(-5, 0))
  assert_eq(0, multiply(0, -5))
  assert_eq(0, multiply(0, 0))
  
  // 测试最小值与零的运算
  assert_eq(-2147483648, add(-2147483648, 0))
  assert_eq(0, multiply(-2147483648, 0))
}

// 测试用例8: 数学性质测试
///|
test "mathematical_properties" {
  // 测试加法交换律: a + b = b + a
  assert_eq(add(10, 20), add(20, 10))
  assert_eq(add(-10, 20), add(20, -10))
  assert_eq(add(-10, -20), add(-20, -10))
  
  // 测试乘法交换律: a * b = b * a
  assert_eq(multiply(10, 20), multiply(20, 10))
  assert_eq(multiply(-10, 20), multiply(20, -10))
  assert_eq(multiply(-10, -20), multiply(-20, -10))
  
  // 测试加法结合律: (a + b) + c = a + (b + c)
  let a = 10
  let b = 20
  let c = 30
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
}

// 测试用例9: 实际应用场景测试
///|
test "practical_application_scenarios" {
  // 场景1: 计算矩形面积和周长
  let length = 10
  let width = 5
  let area = multiply(length, width)  // 面积 = 长 × 宽
  let perimeter = multiply(2, add(length, width))  // 周长 = 2 × (长 + 宽)
  
  assert_eq(50, area)  // 10 × 5 = 50
  assert_eq(30, perimeter)  // 2 × (10 + 5) = 30
  
  // 场景2: 购物车总价计算
  let item1_price = 100
  let item1_quantity = 2
  let item2_price = 50
  let item2_quantity = 3
  let total = add(
    multiply(item1_price, item1_quantity),
    multiply(item2_price, item2_quantity)
  )
  
  assert_eq(350, total)  // 100×2 + 50×3 = 200 + 150 = 350
  
  // 场景3: 温度转换 (简化版，避免除法)
  let celsius = 10
  // 华氏度 = 摄氏度 × 9/5 + 32，这里我们只计算 (摄氏度 × 9 + 160)
  let fahrenheit_times_5 = add(multiply(celsius, 9), 160)
  assert_eq(250, fahrenheit_times_5)  // 10×9 + 160 = 90 + 160 = 250
}

// 测试用例10: 特殊输入和错误处理测试
///|
test "special_inputs_and_error_handling" {
  // 测试极值组合
  assert_eq(-1, add(2147483647, -2147483648))  // 最大值 + 最小值
  assert_eq(-1, add(-2147483648, 2147483647))  // 最小值 + 最大值
  assert_eq(2147483647, add(2147483647, 0))   // 最大值 + 0
  assert_eq(-2147483648, add(-2147483648, 0))  // 最小值 + 0
  
  // 测试乘法中的极值
  assert_eq(-2147483648, multiply(-2147483648, 1))  // 最小值 × 1
  assert_eq(0, multiply(-2147483648, 0))            // 最小值 × 0
  assert_eq(-2147483648, multiply(-2147483648, -1)) // 最小值 × -1 (溢出)
  
  // 测试字符串输入的边界情况
  assert_eq("Hello, !", greet(""))                    // 空字符串
  assert_eq("Hello, a!", greet("a"))                  // 单字符
  assert_eq("Hello, verylongname!", greet("verylongname"))  // 长字符串
}