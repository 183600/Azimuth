// 标准 MoonBit 测试用例
// 覆盖 azimuth 项目的主要功能

test "add_basic_functionality" {
  // 测试基本加法功能
  inspect(@azimuth.add(2, 3), "5")?
  inspect(@azimuth.add(0, 0), "0")?
  inspect(@azimuth.add(-5, 10), "5")?
  inspect(@azimuth.add(-3, -7), "-10")?
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  inspect(@azimuth.multiply(3, 4), "12")?
  inspect(@azimuth.multiply(0, 5), "0")?
  inspect(@azimuth.multiply(1, 7), "7")?
  inspect(@azimuth.multiply(-2, 8), "-16")?
  inspect(@azimuth.multiply(-3, -4), "12")?
}

test "greet_functionality" {
  // 测试问候函数
  inspect(@azimuth.greet("World"), "Hello, World!")?
  inspect(@azimuth.greet("MoonBit"), "Hello, MoonBit!")?
  inspect(@azimuth.greet(""), "Hello, !")?
  inspect(@azimuth.greet("123"), "Hello, 123!")?
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法的基本功能
  inspect(@azimuth.divide_with_ceil(10, 3), "4")?
  inspect(@azimuth.divide_with_ceil(9, 3), "3")?
  inspect(@azimuth.divide_with_ceil(-10, 3), "-3")?
  inspect(@azimuth.divide_with_ceil(10, -3), "-3")?
  inspect(@azimuth.divide_with_ceil(-10, -3), "4")?
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  inspect(divide_with_ceil(0, 5), "0")?
  inspect(divide_with_ceil(5, 0), "0")?  // 除零情况返回0
  inspect(divide_with_ceil(1, 1), "1")?
  inspect(divide_with_ceil(-1, 1), "-1")?
  inspect(divide_with_ceil(1, -1), "-1")?
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 6
  let b = 8
  
  // 加法交换律
  let sum1 = @azimuth.add(a, b)
  let sum2 = @azimuth.add(b, a)
  inspect(sum1, sum2.to_string())?
  
  // 乘法交换律
  let product1 = @azimuth.multiply(a, b)
  let product2 = @azimuth.multiply(b, a)
  inspect(product1, product2.to_string())?
}

test "complex_calculations" {
  // 测试复合计算
  // (2 + 3) * 4 = 20
  let result1 = @azimuth.multiply(@azimuth.add(2, 3), 4)
  inspect(result1, "20")?
  
  // 10 / 3 向上取整 = 4
  let result2 = @azimuth.divide_with_ceil(10, 3)
  inspect(result2, "4")?
  
  // 组合计算：(5 + 3) * (10 / 4 向上取整) = 8 * 3 = 24
  let result3 = @azimuth.multiply(@azimuth.add(5, 3), @azimuth.divide_with_ceil(10, 4))
  inspect(result3, "24")?
}

test "boundary_values" {
  // 测试边界值
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试加法边界
  inspect(@azimuth.add(max_int, 0), "2147483647")?
  inspect(@azimuth.add(min_int, 0), "-2147483648")?
  inspect(@azimuth.add(0, max_int), "2147483647")?
  inspect(@azimuth.add(0, min_int), "-2147483648")?
  
  // 测试乘法边界
  inspect(@azimuth.multiply(max_int, 1), "2147483647")?
  inspect(@azimuth.multiply(min_int, 1), "-2147483648")?
  inspect(@azimuth.multiply(1, max_int), "2147483647")?
  inspect(@azimuth.multiply(1, min_int), "-2147483648")?
  inspect(@azimuth.multiply(0, max_int), "0")?
  inspect(@azimuth.multiply(0, min_int), "0")?
}

test "string_edge_cases" {
  // 测试字符串边界情况
  let long_string = "ThisIsAVeryLongStringForTestingPurposes"
  inspect(@azimuth.greet(long_string), "Hello, ThisIsAVeryLongStringForTestingPurposes!")?
  
  let special_chars = "!@#$%^&*()"
  inspect(@azimuth.greet(special_chars), "Hello, !@#$%^&*()!")?
  
  let unicode_chars = "测试中文"
  inspect(@azimuth.greet(unicode_chars), "Hello, 测试中文!")?
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 场景1：计算购物车总价
  let item_price = 99
  let quantity = 3
  let total = @azimuth.multiply(item_price, quantity)
  inspect(total, "297")?
  
  // 场景2：计算页数（向上取整）
  let total_items = 100
  let items_per_page = 30
  let pages_needed = @azimuth.divide_with_ceil(total_items, items_per_page)
  inspect(pages_needed, "4")?
  
  // 场景3：组合计算
  let base_price = 50
  let tax = 8
  let shipping = 15
  let final_total = @azimuth.add(@azimuth.add(@azimuth.multiply(base_price, 1), tax), shipping)
  inspect(final_total, "73")?
}