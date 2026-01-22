// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_basic_properties" {
  // 测试加法的基本性质
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  
  // 测试加法交换律
  let a = 42
  let b = 17
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 测试加法结合律
  let x = 5
  let y = 10
  let z = 15
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(x, y), z), @azimuth.add(x, @azimuth.add(y, z)))
}

test "multiply_function_basic_properties" {
  // 测试乘法的基本性质
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(-25, @azimuth.multiply(-5, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  
  // 测试乘法交换律
  let a = 6
  let b = 8
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试乘法结合律
  let x = 2
  let y = 3
  let z = 4
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(x, y), z), @azimuth.multiply(x, @azimuth.multiply(y, z)))
  
  // 测试分配律
  @azimuth.assert_eq(@azimuth.multiply(2, @azimuth.add(3, 4)), @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(2, 4)))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, space separated!", @azimuth.greet("space separated"))
}

test "divide_with_ceil_function_comprehensive" {
  // 测试向上取整除法的各种情况
  // 正数除法
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(6, 2))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(7, 2))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(8, 2))
  
  // 负数除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-6, 2))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-7, 2))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-8, 2))
  
  // 除数为负数
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(6, -2))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(7, -2))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-6, -2))
  
  // 零除数
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
}

test "boundary_values_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法边界测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 乘法边界测试
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 除零测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(min_val, 0))
}

test "complex_arithmetic_expressions" {
  // 测试复杂的算术表达式
  // (a + b) * c
  let a = 5
  let b = 3
  let c = 4
  let sum = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(32, product)
  
  // a * b + c * d
  let x = 2
  let y = 3
  let z = 4
  let w = 5
  let product1 = @azimuth.multiply(x, y)
  let product2 = @azimuth.multiply(z, w)
  let result = @azimuth.add(product1, product2)
  @azimuth.assert_eq(26, result)
  
  // 嵌套运算
  let nested = @azimuth.multiply(@azimuth.add(1, 2), @azimuth.add(3, 4))
  @azimuth.assert_eq(21, nested)
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  
  // 场景1：计算购物车总价
  let item1 = 199
  let item2 = 299
  let item3 = 99
  let subtotal1 = @azimuth.add(item1, item2)
  let total = @azimuth.add(subtotal1, item3)
  @azimuth.assert_eq(597, total)
  
  // 场景2：计算订单数量（单价 × 数量）
  let unit_price = 25
  let quantity = 8
  let order_total = @azimuth.multiply(unit_price, quantity)
  @azimuth.assert_eq(200, order_total)
  
  // 场景3：计算平均分配（向上取整）
  let total_items = 17
  let groups = 5
  let items_per_group = @azimuth.divide_with_ceil(total_items, groups)
  @azimuth.assert_eq(4, items_per_group)
  
  // 场景4：生成用户欢迎消息
  let user_name = "李四"
  let welcome_msg = @azimuth.greet(user_name)
  @azimuth.assert_eq_string("Hello, 李四!", welcome_msg)
}

test "edge_cases_and_special_values" {
  // 测试边缘情况和特殊值
  
  // 零值运算
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  
  // 单位元运算
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(-42, @azimuth.multiply(-1, 42))
  @azimuth.assert_eq(-42, @azimuth.multiply(42, -1))
  
  // 自反运算
  @azimuth.assert_eq(0, @azimuth.add(42, -42))
  @azimuth.assert_eq(0, @azimuth.add(-42, 42))
  
  // 字符串边缘情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(" "))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  
  // 加法单位元
  let test_values = [0, 1, -1, 42, -42, 100, -100]
  for value in test_values {
    @azimuth.assert_eq(value, @azimuth.add(value, 0))
    @azimuth.assert_eq(value, @azimuth.add(0, value))
  }
  
  // 乘法单位元
  for value in test_values {
    @azimuth.assert_eq(value, @azimuth.multiply(value, 1))
    @azimuth.assert_eq(value, @azimuth.multiply(1, value))
  }
  
  // 乘法零元
  for value in test_values {
    @azimuth.assert_eq(0, @azimuth.multiply(value, 0))
    @azimuth.assert_eq(0, @azimuth.multiply(0, value))
  }
  
  // 加法逆元
  for value in test_values {
    @azimuth.assert_eq(0, @azimuth.add(value, -value))
    @azimuth.assert_eq(0, @azimuth.add(-value, value))
  }
}