// 标准 MoonBit 测试套件
// 测试 azimuth 包的核心功能

test "add_function_basic_operations" {
  // 基本加法操作测试
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-5, add(-2, -3))
  assert_eq(0, add(10, -10))
  assert_eq(100, add(50, 50))
}

test "add_function_overflow_handling" {
  // 加法溢出处理测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正溢出测试
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  
  // 负溢出测试
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
  
  // 边界值测试
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
}

test "multiply_function_basic_operations" {
  // 基本乘法操作测试
  assert_eq(0, multiply(0, 5))
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(25, multiply(5, 5))
  assert_eq(100, multiply(10, 10))
}

test "multiply_function_special_cases" {
  // 乘法特殊情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 单位元测试
  assert_eq(5, multiply(5, 1))
  assert_eq(5, multiply(1, 5))
  assert_eq(-5, multiply(-5, 1))
  
  // 零元测试
  assert_eq(0, multiply(0, max_val))
  assert_eq(0, multiply(0, min_val))
  
  // 特殊值测试
  assert_eq(min_val, multiply(-1, min_val))
  assert_eq(max_val, multiply(-1, -max_val))
}

test "greet_function_comprehensive" {
  // greet 函数综合测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  // 正数测试
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(2, divide_with_ceil(5, 3))
  
  // 负数测试
  assert_eq(-3, divide_with_ceil(-8, 3))
  assert_eq(-2, divide_with_ceil(-6, 3))
  assert_eq(-1, divide_with_ceil(-2, 3))
  
  // 零测试
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -5))
}

test "combined_operations_complex" {
  // 组合运算复杂测试
  // 多层嵌套运算
  assert_eq(25, add(multiply(3, 5), multiply(2, 5)))
  assert_eq(20, multiply(add(3, 2), 4))
  assert_eq(16, add(multiply(2, 5), divide_with_ceil(10, 3)))
  
  // 复杂表达式
  let result = add(multiply(divide_with_ceil(15, 4), 2), multiply(3, 2))
  assert_eq(18, result)
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  // 加法交换律
  assert_eq(add(5, 3), add(3, 5))
  assert_eq(add(-5, 3), add(3, -5))
  
  // 乘法交换律
  assert_eq(multiply(5, 3), multiply(3, 5))
  assert_eq(multiply(-5, 3), multiply(3, -5))
  
  // 分配律
  assert_eq(multiply(2, add(3, 4)), add(multiply(2, 3), multiply(2, 4)))
}

test "real_world_scenarios" {
  // 实际应用场景测试
  // 场景1：计算购物车总价
  let item1 = 199
  let item2 = 299
  let subtotal = add(item1, item2)
  let tax = divide_with_ceil(subtotal, 10)  // 10% 税
  let total = add(subtotal, tax)
  assert_eq(548, total)
  
  // 场景2：批量计算
  let unit_price = 15
  let quantity = 7
  let discount = 2  // 50% 折扣
  let discounted_price = divide_with_ceil(multiply(unit_price, quantity), discount)
  assert_eq(53, discounted_price)
  
  // 场景3：用户欢迎消息
  let username = "李四"
  let welcome = greet(username)
  assert_eq_string("Hello, 李四!", welcome)
}