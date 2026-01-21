// Azimuth 简洁测试套件
// 使用标准的 MoonBit 测试语法，包含10个核心测试用例

test "basic_addition" {
  // 测试基本加法运算
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-2, 2))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(100, @azimuth.add(1, 99))
}

test "basic_multiplication" {
  // 测试基本乘法运算
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(8, 0))
}

test "string_greeting" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "ceil_division" {
  // 测试向上取整除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
}

test "boolean_assertions" {
  // 测试布尔断言
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_false(1 == 0)
  @azimuth.assert_false(3 > 5)
}

test "complex_operations" {
  // 测试复合运算
  let a = 10
  let b = 20
  let c = -5
  
  // (a * b) + c
  let product = @azimuth.multiply(a, b)
  let result = @azimuth.add(product, c)
  @azimuth.assert_eq(195, result)
  
  // a * (b + c)
  let sum = @azimuth.add(b, c)
  let result2 = @azimuth.multiply(a, sum)
  @azimuth.assert_eq(150, result2)
}

test "boundary_values" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "error_handling" {
  // 测试错误处理
  // 除零处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
}

test "mathematical_properties" {
  // 测试数学性质
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(123, 456), @azimuth.add(456, 123))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(12, 34), @azimuth.multiply(34, 12))
  
  // 分配律（在安全范围内）
  let a = 10
  let b = 20
  let c = 30
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}

test "practical_scenarios" {
  // 测试实际应用场景
  // 场景1：计算购物车总价
  let item1 = 299
  let item2 = 399
  let subtotal = @azimuth.add(item1, item2)
  let total = @azimuth.multiply(subtotal, 2)  // 假设购买2份
  @azimuth.assert_eq(1396, total)
  
  // 场景2：计算平均分（向上取整）
  let score1 = 87
  let score2 = 93
  let score3 = 79
  let total_score = @azimuth.add(@azimuth.add(score1, score2), score3)
  let average = @azimuth.divide_with_ceil(total_score, 3)
  @azimuth.assert_eq(87, average)
  
  // 场景3：用户欢迎消息
  let username = "Alice"
  let welcome = @azimuth.greet(username)
  @azimuth.assert_eq_string("Hello, Alice!", welcome)
}