// 简洁的标准 MoonBit 测试用例
// 覆盖 azimuth 项目的主要功能，不超过10个测试用例

test "add_function_basic" {
  // 测试基本加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-1, 1))
  @azimuth.assert_eq(-8, @azimuth.add(-5, -3))
  @azimuth.assert_eq(100, @azimuth.add(0, 100))
}

test "multiply_function_basic" {
  // 测试基本乘法功能
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(-10, @azimuth.multiply(-2, 5))
  @azimuth.assert_eq(9, @azimuth.multiply(-3, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 50))
  @azimuth.assert_eq(7, @azimuth.multiply(7, 1))
}

test "greet_function_basic" {
  // 测试基本问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "divide_with_ceil_function" {
  // 测试向上取整除法功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "arithmetic_properties" {
  // 测试算术性质
  let a = 6
  let b = 8
  
  // 加法交换律
  let sum1 = @azimuth.add(a, b)
  let sum2 = @azimuth.add(b, a)
  @azimuth.assert_eq(sum1, sum2)
  
  // 乘法交换律
  let product1 = @azimuth.multiply(a, b)
  let product2 = @azimuth.multiply(b, a)
  @azimuth.assert_eq(product1, product2)
  
  // 零元素性质
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
}

test "edge_cases_handling" {
  // 测试边界情况处理
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 边界值测试
  @azimuth.assert_eq(2147483647, @azimuth.add(max_int, 0))
  @azimuth.assert_eq(-2147483648, @azimuth.add(min_int, 0))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(max_int, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(min_int, 1))
  
  // 特殊值测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
}

test "string_operations" {
  // 测试字符串操作
  let long_string = "VeryLongName"
  @azimuth.assert_eq_string("Hello, VeryLongName!", @azimuth.greet(long_string))
  
  let special_chars = "!@#$%"
  @azimuth.assert_eq_string("Hello, !@#$%!", @azimuth.greet(special_chars))
  
  let unicode_chars = "测试"
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet(unicode_chars))
  
  let spaced_name = "John Doe"
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet(spaced_name))
}

test "complex_calculations" {
  // 测试复合计算
  // (2 + 3) * 4 = 20
  let result1 = @azimuth.multiply(@azimuth.add(2, 3), 4)
  @azimuth.assert_eq(20, result1)
  
  // 组合计算：(5 + 3) * (10 / 4 向上取整) = 8 * 3 = 24
  let result2 = @azimuth.multiply(@azimuth.add(5, 3), @azimuth.divide_with_ceil(10, 4))
  @azimuth.assert_eq(24, result2)
  
  // 连续加法：1 + 2 + 3 + 4 + 5 = 15
  let sum1 = @azimuth.add(1, 2)
  let sum2 = @azimuth.add(sum1, 3)
  let sum3 = @azimuth.add(sum2, 4)
  let total = @azimuth.add(sum3, 5)
  @azimuth.assert_eq(15, total)
}

test "error_handling" {
  // 测试错误处理
  // 除零情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 溢出保护（如果实现有的话）
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 1))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(2147483647, 2))
}

test "real_world_applications" {
  // 测试实际应用场景
  // 场景1：计算购物车总价
  let item_price = 99
  let quantity = 3
  let total = @azimuth.multiply(item_price, quantity)
  @azimuth.assert_eq(297, total)
  
  // 场景2：计算需要的页数（向上取整）
  let total_items = 100
  let items_per_page = 30
  let pages_needed = @azimuth.divide_with_ceil(total_items, items_per_page)
  @azimuth.assert_eq(4, pages_needed)
  
  // 场景3：生成个性化问候
  let customer_name = "Alice"
  let greeting = @azimuth.greet(customer_name)
  @azimuth.assert_eq_string("Hello, Alice!", greeting)
  
  // 场景4：计算折扣价
  let original_price = 200
  let discount = -50
  let final_price = @azimuth.add(original_price, discount)
  @azimuth.assert_eq(150, final_price)
}