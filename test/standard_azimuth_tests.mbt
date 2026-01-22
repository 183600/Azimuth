// 标准 MoonBit 测试用例 - 针对 azimuth 库的10个高质量测试
// 覆盖基本功能、边界情况、数学性质和实际应用场景

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(8, add(5, 3))
  assert_eq(15, multiply(3, 5))
  assert_eq(2, divide_with_ceil(5, 3))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "mathematical_properties_commutative" {
  // 测试数学交换律性质
  let a = 12
  let b = 8
  assert_eq(add(a, b), add(b, a))  // 加法交换律
  assert_eq(multiply(a, b), multiply(b, a))  // 乘法交换律
}

test "mathematical_properties_identity" {
  // 测试数学单位元性质
  assert_eq(7, add(7, 0))  // 加法单位元
  assert_eq(7, multiply(7, 1))  // 乘法单位元
  assert_eq(0, multiply(7, 0))  // 乘法零元
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))   // 10/3 = 3.33... -> 4
  assert_eq(3, divide_with_ceil(9, 3))    // 9/3 = 3 -> 3
  assert_eq(-3, divide_with_ceil(-10, 3)) // -10/3 = -3.33... -> -3
  assert_eq(0, divide_with_ceil(5, 0))    // 除数为0的安全处理
  assert_eq(1, divide_with_ceil(1, 100))  // 小数向上取整
}

test "boundary_value_testing" {
  // 边界值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(0, -1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq_string("Hello, !", greet(""))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax)
  assert_eq(231, total)
}

test "string_processing_multilingual" {
  // 多语言字符串处理测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, María!", greet("María"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, 12345!", greet("12345"))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "large_scale_computations" {
  // 大规模计算测试
  let big_num = 100000
  assert_eq(200000, add(big_num, big_num))
  assert_eq(10000000000, multiply(big_num, big_num))
  assert_eq(33334, divide_with_ceil(1000000, 30))
  
  // 复杂大数计算
  let complex_result = add(
    multiply(1000, divide_with_ceil(100000, 7)),
    multiply(500, divide_with_ceil(50000, 3))
  )
  assert_eq(28500000, complex_result)
}

test "real_world_inventory_management" {
  // 实际库存管理场景
  let initial_stock = 500
  let daily_sales = 25
  let days_in_month = 30
  let restock_batch = 300
  
  // 计算月销售量
  let monthly_sales = multiply(daily_sales, days_in_month)
  // 计算月底库存
  let month_end_stock = add(initial_stock, -monthly_sales)
  // 计算补货后的库存
  let final_stock = add(month_end_stock, restock_batch)
  // 计算需要的存储箱数（每箱装50个）
  let storage_boxes = divide_with_ceil(final_stock, 50)
  
  assert_eq(750, monthly_sales)
  assert_eq(-250, month_end_stock)
  assert_eq(50, final_stock)
  assert_eq(1, storage_boxes)
}