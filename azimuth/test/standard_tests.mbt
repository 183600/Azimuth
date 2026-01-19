// 标准 MoonBit 测试用例 - 覆盖 azimuth 包的核心功能

test "add_basic_functionality" {
  // 测试基本的加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-3, 3))
  @azimuth.assert_eq(-10, @azimuth.add(-7, -3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "add_boundary_values" {
  // 测试边界值的加法
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试零值处理
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(10, @azimuth.add(0, 10))
  @azimuth.assert_eq(-10, @azimuth.add(-10, 0))
  
  // 测试边界值处理
  @azimuth.assert_eq(max_int, @azimuth.add(max_int, 0))
  @azimuth.assert_eq(min_int, @azimuth.add(min_int, 0))
  @azimuth.assert_eq(max_int, @azimuth.add(max_int, 1))
  @azimuth.assert_eq(min_int, @azimuth.add(min_int, -1))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(15, @azimuth.multiply(1, 15))
  @azimuth.assert_eq(15, @azimuth.multiply(15, 1))
}

test "multiply_boundary_values" {
  // 测试边界值的乘法
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试零值和一值
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_int))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_int))
  @azimuth.assert_eq(max_int, @azimuth.multiply(1, max_int))
  @azimuth.assert_eq(min_int, @azimuth.multiply(1, min_int))
  
  // 测试-1的特殊情况
  @azimuth.assert_eq(-max_int, @azimuth.multiply(-1, max_int))
  @azimuth.assert_eq(min_int, @azimuth.multiply(-1, min_int))
  
  // 测试溢出保护
  @azimuth.assert_eq(max_int, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(min_int, @azimuth.multiply(-46341, 46341))
}

test "greet_functionality" {
  // 测试基本的问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
}

test "greet_special_characters" {
  // 测试包含特殊字符的问候
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
  @azimuth.assert_eq_string("Hello, \"quoted\"!", @azimuth.greet("\"quoted\""))
  @azimuth.assert_eq_string("Hello, \\path\\!", @azimuth.greet("\\path\\"))
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  // (2 + 3) * (4 + 5) = 5 * 9 = 45
  let sum1 = @azimuth.add(2, 3)
  let sum2 = @azimuth.add(4, 5)
  let result1 = @azimuth.multiply(sum1, sum2)
  @azimuth.assert_eq(45, result1)
  
  // ((10 * 5) + (20 * 3)) - (15 * 2) = (50 + 60) - 30 = 80
  let product1 = @azimuth.multiply(10, 5)
  let product2 = @azimuth.multiply(20, 3)
  let sum3 = @azimuth.add(product1, product2)
  let product3 = @azimuth.multiply(15, 2)
  let result2 = @azimuth.add(sum3, -product3)
  @azimuth.assert_eq(80, result2)
}

test "edge_case_combinations" {
  // 测试边界情况的组合
  let max_int = 2147483647
  let min_int = -2147483648
  
  // (max_int + 0) * 1 = max_int
  let result1 = @azimuth.multiply(@azimuth.add(max_int, 0), 1)
  @azimuth.assert_eq(max_int, result1)
  
  // (min_int + 0) * -1 = min_int
  let result2 = @azimuth.multiply(@azimuth.add(min_int, 0), -1)
  @azimuth.assert_eq(min_int, result2)
  
  // (0 + 1) * (max_int - (max_int - 1)) = 1 * 1 = 1
  let result3 = @azimuth.multiply(@azimuth.add(0, 1), @azimuth.add(max_int, -max_int + 1))
  @azimuth.assert_eq(1, result3)
}

test "practical_application_scenarios" {
  // 场景1：计算购物车总价
  let book_price = 299
  let pen_price = 15
  let notebook_price = 45
  let shipping = 20
  let total = @azimuth.add(@azimuth.add(@azimuth.add(book_price, pen_price), notebook_price), shipping)
  @azimuth.assert_eq(379, total)
  
  // 场景2：计算批量订单总价
  let unit_price = 125
  let quantity = 8
  let order_total = @azimuth.multiply(unit_price, quantity)
  @azimuth.assert_eq(1000, order_total)
  
  // 场景3：计算折扣后的价格（原价 * 折扣系数）
  let original_price = 500
  let discount_factor = 8  // 80%的价格
  let discounted_price = @azimuth.multiply(original_price, discount_factor)
  @azimuth.assert_eq(4000, discounted_price)
}

test "assertion_helpers" {
  // 测试断言辅助函数
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_true(@azimuth.add(2, 3) == 5)
  @azimuth.assert_true(@azimuth.multiply(4, 5) == 20)
  @azimuth.assert_true(@azimuth.greet("Test") == "Hello, Test!")
  
  @azimuth.assert_false(1 == 2)
  @azimuth.assert_false(@azimuth.add(2, 3) == 6)
  @azimuth.assert_false(@azimuth.multiply(4, 5) == 21)
  @azimuth.assert_false(@azimuth.greet("Test") == "Hi, Test!")
}