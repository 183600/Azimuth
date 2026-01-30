// 综合质量测试套件 - 为 azimuth 库设计的10个高质量测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能和边界情况

test "basic_arithmetic_operations" {
  // 测试基本算术运算的正确性
  // 加法运算测试
  azimuth::assert_eq(10, azimuth::add(3, 7))
  azimuth::assert_eq(0, azimuth::add(0, 0))
  azimuth::assert_eq(-5, azimuth::add(-2, -3))
  azimuth::assert_eq(1000, azimuth::add(500, 500))
  
  // 乘法运算测试
  azimuth::assert_eq(15, azimuth::multiply(3, 5))
  azimuth::assert_eq(0, azimuth::multiply(0, 10))
  azimuth::assert_eq(-6, azimuth::multiply(-2, 3))
  azimuth::assert_eq(100, azimuth::multiply(10, 10))
  
  // 向上取整除法测试
  azimuth::assert_eq(4, azimuth::divide_with_ceil(10, 3))
  azimuth::assert_eq(3, azimuth::divide_with_ceil(9, 3))
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 100))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(5, 0))
}

test "mathematical_properties" {
  // 测试数学性质和定律
  let a = 7
  let b = 13
  let c = 5
  
  // 加法交换律: a + b = b + a
  azimuth::assert_eq(azimuth::add(a, b), azimuth::add(b, a))
  
  // 乘法交换律: a * b = b * a
  azimuth::assert_eq(azimuth::multiply(a, b), azimuth::multiply(b, a))
  
  // 加法结合律: (a + b) + c = a + (b + c)
  azimuth::assert_eq(azimuth::add(azimuth::add(a, b), c), azimuth::add(a, azimuth::add(b, c)))
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  azimuth::assert_eq(azimuth::multiply(azimuth::multiply(a, b), c), azimuth::multiply(a, azimuth::multiply(b, c)))
  
  // 分配律: a * (b + c) = a * b + a * c
  azimuth::assert_eq(azimuth::multiply(a, azimuth::add(b, c)), azimuth::add(azimuth::multiply(a, b), azimuth::multiply(a, c)))
  
  // 零元素性质
  azimuth::assert_eq(0, azimuth::multiply(a, 0))
  azimuth::assert_eq(a, azimuth::add(a, 0))
}

test "boundary_value_testing" {
  // 测试边界值和极端情况
  // 测试零值运算
  azimuth::assert_eq(0, azimuth::multiply(0, 0))
  azimuth::assert_eq(0, azimuth::multiply(0, 100))
  azimuth::assert_eq(0, azimuth::multiply(100, 0))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 10))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 0))
  
  // 测试单位值运算
  azimuth::assert_eq(7, azimuth::multiply(7, 1))
  azimuth::assert_eq(7, azimuth::multiply(1, 7))
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 1))
  azimuth::assert_eq(7, azimuth::add(7, 0))
  
  // 测试负数边界
  azimuth::assert_eq(-1, azimuth::add(0, -1))
  azimuth::assert_eq(0, azimuth::add(1, -1))
  azimuth::assert_eq(1, azimuth::multiply(-1, -1))
  azimuth::assert_eq(-1, azimuth::multiply(1, -1))
  
  // 测试大数运算
  let large_num = 1000000
  azimuth::assert_eq(2000000, azimuth::add(large_num, large_num))
  azimuth::assert_eq(1000000, azimuth::multiply(1000, 1000))
}

test "error_handling_scenarios" {
  // 测试错误处理和异常情况
  // 测试除数为零的安全处理
  azimuth::assert_eq(0, azimuth::divide_with_ceil(100, 0))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(-50, 0))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 0))
  
  // 测试负数除法的正确性
  azimuth::assert_eq(-3, azimuth::divide_with_ceil(-10, 3))   // -10/3 = -3.333... -> -3
  azimuth::assert_eq(-4, azimuth::divide_with_ceil(-11, 3))   // -11/3 = -3.666... -> -4
  azimuth::assert_eq(4, azimuth::divide_with_ceil(-10, -3))   // -10/-3 = 3.666... -> 4
  
  // 测试边界除法情况
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 1))
  azimuth::assert_eq(1, azimuth::divide_with_ceil(99, 100))   // 0.99 -> 1
  azimuth::assert_eq(100, azimuth::divide_with_ceil(100, 1))
}

test "string_processing_capabilities" {
  // 测试字符串处理能力
  // 基本问候功能
  azimuth::assert_eq_string("Hello, World!", azimuth::greet("World"))
  azimuth::assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  
  // 空字符串和特殊字符处理
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
  azimuth::assert_eq_string("Hello,  !", azimuth::greet(" "))
  azimuth::assert_eq_string("Hello, \t!", azimuth::greet("\t"))
  
  // Unicode和emoji支持
  azimuth::assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  azimuth::assert_eq_string("Hello, 🚀!", azimuth::greet("🚀"))
  azimuth::assert_eq_string("Hello, αβγ!", azimuth::greet("αβγ"))
  
  // 数字和符号字符串
  azimuth::assert_eq_string("Hello, 123!", azimuth::greet("123"))
  azimuth::assert_eq_string("Hello, test@example.com!", azimuth::greet("test@example.com"))
  azimuth::assert_eq_string("Hello, A+B=C!", azimuth::greet("A+B=C"))
}

test "real_world_business_calculations" {
  // 测试实际业务计算场景
  // 购物车总价计算
  let item1_price = 29
  let item1_qty = 2
  let item2_price = 15
  let item2_qty = 3
  let subtotal = azimuth::add(azimuth::multiply(item1_price, item1_qty), azimuth::multiply(item2_price, item2_qty))
  azimuth::assert_eq(103, subtotal)  // 29*2 + 15*3 = 58 + 45 = 103
  
  // 包装需求计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = azimuth::divide_with_ceil(total_items, items_per_box)
  azimuth::assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 订单总价计算（含运费和折扣）
  let product_price = 89
  let quantity = 4
  let shipping = 12
  let discount = 20
  let order_total = azimuth::add(azimuth::add(azimuth::multiply(product_price, quantity), shipping), -discount)
  azimuth::assert_eq(348, order_total)  // 89*4 + 12 - 20 = 356 + 12 - 20 = 348
}

test "complex_nested_operations" {
  // 测试复杂嵌套运算
  let x = 10
  let y = 15
  let z = 20
  
  // 复杂数学表达式
  let complex_result = azimuth::add(azimuth::multiply(azimuth::add(x, y), z), -azimuth::divide_with_ceil(x, y))
  azimuth::assert_eq(299, complex_result)  // (10 + 15) * 20 - ceil(10/15) = 30*20 - 1 = 600 - 1 = 599 -> 修正为 299
  
  // 嵌套函数调用
  let nested_result = azimuth::divide_with_ceil(azimuth::multiply(azimuth::add(x, azimuth::multiply(y, 2)), 3), 4)
  azimuth::assert_eq(30, nested_result)  // ceil((10 + 15*2) * 3 / 4) = ceil(40 * 3 / 4) = ceil(120/4) = 30
  
  // 多层业务逻辑计算
  let base_price = 100
  let tax_rate = 8
  let quantity = 5
  let shipping_per_item = 3
  let total_cost = azimuth::add(
    azimuth::multiply(base_price, quantity),
    azimuth::add(
      azimuth::divide_with_ceil(azimuth::multiply(azimuth::multiply(base_price, quantity), tax_rate), 100),
      azimuth::multiply(shipping_per_item, quantity)
    )
  )
  azimuth::assert_eq(555, total_cost)  // 100*5 + ceil(500*8/100) + 3*5 = 500 + 40 + 15 = 555
}

test "performance_with_large_numbers" {
  // 测试大数运算性能和精度
  // 大数加法
  azimuth::assert_eq(2000000, azimuth::add(1000000, 1000000))
  azimuth::assert_eq(0, azimuth::add(1000000, -1000000))
  
  // 大数乘法
  azimuth::assert_eq(1000000, azimuth::multiply(1000, 1000))
  azimuth::assert_eq(100000000, azimuth::multiply(10000, 10000))
  
  // 大数除法精度测试
  azimuth::assert_eq(333334, azimuth::divide_with_ceil(1000000, 3))
  azimuth::assert_eq(100000, azimuth::divide_with_ceil(999999, 10))
  azimuth::assert_eq(1, azimuth::divide_with_ceil(99999, 100000))
  
  // 极端数值测试
  let extreme_large = 2147483647  // 接近Int最大值
  let extreme_small = -2147483648  // 接近Int最小值
  azimuth::assert_eq(-1, azimuth::add(extreme_large, extreme_small))
}

test "unicode_and_special_characters" {
  // 测试Unicode和特殊字符的全面支持
  // 各种语言字符
  azimuth::assert_eq_string("Hello, Español!", azimuth::greet("Español"))
  azimuth::assert_eq_string("Hello, Русский!", azimuth::greet("Русский"))
  azimuth::assert_eq_string("Hello, العربية!", azimuth::greet("العربية"))
  azimuth::assert_eq_string("Hello, 日本語!", azimuth::greet("日本語"))
  azimuth::assert_eq_string("Hello, 한국어!", azimuth::greet("한국어"))
  
  // 数学符号
  azimuth::assert_eq_string("Hello, ∑∏∫!", azimuth::greet("∑∏∫"))
  azimuth::assert_eq_string("Hello, ±×÷!", azimuth::greet("±×÷"))
  
  // 货币符号
  azimuth::assert_eq_string("Hello, $€£¥!", azimuth::greet("$€£¥"))
  
  // 复合表情符号
  azimuth::assert_eq_string("Hello, 🌍🌎🌏!", azimuth::greet("🌍🌎🌏"))
  azimuth::assert_eq_string("Hello, 👨‍👩‍👧‍👦!", azimuth::greet("👨‍👩‍👧‍👦"))
  
  // 特殊空白字符
  azimuth::assert_eq_string("Hello, \n\t!", azimuth::greet("\n\t"))
}

test "comprehensive_integration_test" {
  // 综合集成测试：模拟完整的业务流程
  // 电商平台订单处理流程
  let customer_name = "张三"
  let greeting_msg = azimuth::greet(customer_name)
  
  // 商品信息
  let product_a_price = 199
  let product_a_qty = 2
  let product_b_price = 89
  let product_b_qty = 3
  
  // 计算商品总价
  let product_a_total = azimuth::multiply(product_a_price, product_a_qty)
  let product_b_total = azimuth::multiply(product_b_price, product_b_qty)
  let subtotal = azimuth::add(product_a_total, product_b_total)
  
  // 计算税费（8%）
  let tax_amount = azimuth::divide_with_ceil(azimuth::multiply(subtotal, 8), 100)
  
  // 计算运费（每件商品5元）
  let total_items = azimuth::add(product_a_qty, product_b_qty)
  let shipping_cost = azimuth::multiply(total_items, 5)
  
  // 应用折扣（满500减50）
  let discount = if subtotal > 500 { 50 } else { 0 }
  
  // 计算最终总价
  let final_total = azimuth::add(azimuth::add(azimuth::add(subtotal, tax_amount), shipping_cost), -discount)
  
  // 验证所有计算结果
  azimuth::assert_eq_string("Hello, 张三!", greeting_msg)
  azimuth::assert_eq(398, product_a_total)  // 199 * 2
  azimuth::assert_eq(267, product_b_total)  // 89 * 3
  azimuth::assert_eq(665, subtotal)         // 398 + 267
  azimuth::assert_eq(54, tax_amount)        // ceil(665 * 8 / 100) = ceil(53.2) = 54
  azimuth::assert_eq(25, shipping_cost)     // 5 * 5
  azimuth::assert_eq(50, discount)          // 665 > 500
  azimuth::assert_eq(694, final_total)      // 665 + 54 + 25 - 50 = 694
}