// Azimuth 增强标准测试用例 - 10个高质量测试用例
// 使用标准的 MoonBit 测试语法

test "add_function_boundary_cases" {
  // 加法函数边界情况测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(0, @azimuth.add(50, -50))
  @azimuth.assert_eq(1, @azimuth.add(2147483647, -2147483646))  // 接近整数边界
}

test "multiply_function_edge_cases" {
  // 乘法函数边界情况测试
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(-1, 1))
  @azimuth.assert_eq(1, @azimuth.multiply(-1, -1))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
}

test "divide_with_ceil_comprehensive_cases" {
  // 向上取整除法综合边界情况测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 5))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(5, 3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-5, 5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-5, -5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))  // 除零情况
}

test "subtract_function_comprehensive" {
  // 减法函数综合测试
  @azimuth.assert_eq(0, @azimuth.subtract(5, 5))
  @azimuth.assert_eq(10, @azimuth.subtract(15, 5))
  @azimuth.assert_eq(-10, @azimuth.subtract(5, 15))
  @azimuth.assert_eq(5, @azimuth.subtract(0, -5))
  @azimuth.assert_eq(-5, @azimuth.subtract(-5, 0))
  @azimuth.assert_eq(0, @azimuth.subtract(-5, -5))
}

test "greet_function_special_cases" {
  // 问候函数特殊情况测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))  // 空格字符串
}

test "complex_arithmetic_combinations" {
  // 复杂算术组合测试
  let a = 10
  let b = 3
  let c = -2
  
  // 测试多种运算组合
  @azimuth.assert_eq(13, @azimuth.add(a, b))
  @azimuth.assert_eq(8, @azimuth.subtract(a, b))
  @azimuth.assert_eq(30, @azimuth.multiply(a, b))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(a, b))
  
  // 包含负数的组合
  @azimuth.assert_eq(8, @azimuth.add(a, c))
  @azimuth.assert_eq(12, @azimuth.subtract(a, c))
  @azimuth.assert_eq(-20, @azimuth.multiply(a, c))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(a, c))
}

test "identity_element_properties" {
  // 单位元性质测试
  let x = 42
  
  // 加法单位元
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(x, @azimuth.add(0, x))
  
  // 乘法单位元
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(x, @azimuth.multiply(1, x))
  
  // 减法单位元
  @azimuth.assert_eq(0, @azimuth.subtract(x, x))
}

test "error_handling_scenarios" {
  // 错误处理场景测试
  // 除零处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 大数处理
  let big_num = 1000000
  @azimuth.assert_eq(2000000, @azimuth.add(big_num, big_num))
  @azimuth.assert_eq(1000000000000, @azimuth.multiply(big_num, big_num))
}

test "string_processing_validation" {
  // 字符串处理验证测试
  // 验证每个名称都能正确处理
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
  @azimuth.assert_eq_string("Hello, Charlie!", @azimuth.greet("Charlie"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
}

test "real_world_application_scenarios" {
  // 真实世界应用场景测试
  // 场景1：计算购物车总价
  let item1_price = 25
  let item2_price = 30
  let item3_price = 15
  let total_price = @azimuth.add(@azimuth.add(item1_price, item2_price), item3_price)
  @azimuth.assert_eq(70, total_price)
  
  // 场景2：计算需要的容器数量
  let total_items = 23
  let container_capacity = 5
  let containers_needed = @azimuth.divide_with_ceil(total_items, container_capacity)
  @azimuth.assert_eq(5, containers_needed)
  
  // 场景3：计算折扣后的价格
  let original_price = 100
  let discount = 20
  let final_price = @azimuth.subtract(original_price, discount)
  @azimuth.assert_eq(80, final_price)
  
  // 场景4：批量订单计算
  let unit_price = 12
  let quantity = 8
  let subtotal = @azimuth.multiply(unit_price, quantity)
  @azimuth.assert_eq(96, subtotal)
}