// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_basic_tests" {
  // 测试基本加法功能
  inspect(@azimuth.add(2, 3), "5")?
  inspect(@azimuth.add(-1, 1), "0")?
  inspect(@azimuth.add(0, 0), "0")?
}

test "multiply_function_basic_tests" {
  // 测试基本乘法功能
  inspect(@azimuth.multiply(3, 4), "12")?
  inspect(@azimuth.multiply(-2, 3), "-6")?
  inspect(@azimuth.multiply(0, 5), "0")?
}

test "greet_function_basic_tests" {
  // 测试问候函数
  inspect(@azimuth.greet("World"), "Hello, World!")?
  inspect(@azimuth.greet("MoonBit"), "Hello, MoonBit!")?
  inspect(@azimuth.greet(""), "Hello, !")?
}

test "divide_with_ceil_positive_tests" {
  // 测试向上取整除法 - 正数情况
  inspect(@azimuth.divide_with_ceil(10, 3), "4")?
  inspect(@azimuth.divide_with_ceil(9, 3), "3")?
  inspect(@azimuth.divide_with_ceil(1, 2), "1")?
}

test "divide_with_ceil_negative_tests" {
  // 测试向上取整除法 - 负数情况
  inspect(@azimuth.divide_with_ceil(-10, 3), "-3")?
  inspect(@azimuth.divide_with_ceil(10, -3), "-3")?
  inspect(@azimuth.divide_with_ceil(-10, -3), "4")?
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法 - 边界情况
  inspect(@azimuth.divide_with_ceil(0, 5), "0")?
  inspect(@azimuth.divide_with_ceil(5, 0), "0")?
  inspect(@azimuth.divide_with_ceil(1, 1), "1")?
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 5
  let b = 7
  
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
  
  // 组合计算：(5 + 3) * (10 / 4 向上取整) = 8 * 3 = 24
  let result2 = @azimuth.multiply(@azimuth.add(5, 3), @azimuth.divide_with_ceil(10, 4))
  inspect(result2, "24")?
}

test "boundary_values" {
  // 测试边界值
  inspect(@azimuth.add(2147483647, 0), "2147483647")?
  inspect(@azimuth.multiply(2147483647, 1), "2147483647")?
  inspect(@azimuth.multiply(-2147483648, 0), "0")?
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 计算购物车总价
  let item_price = 99
  let quantity = 3
  let total = @azimuth.multiply(item_price, quantity)
  inspect(total, "297")?
  
  // 计算页数（向上取整）
  let total_items = 100
  let items_per_page = 30
  let pages_needed = @azimuth.divide_with_ceil(total_items, items_per_page)
  inspect(pages_needed, "4")?
}