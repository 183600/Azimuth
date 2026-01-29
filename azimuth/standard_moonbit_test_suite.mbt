// 标准 MoonBit 测试套件
// 包含10个高质量的测试用例，覆盖核心功能模块

// 测试1: Int 模块基本功能
test "int_module_basic_functions" {
  // 测试 Int 模块的基本常量和函数
  assert_eq(2147483647, Int::max_value)
  assert_eq(-2147483648, Int::min_value)
  assert_eq(42, Int::abs(42))
  assert_eq(42, Int::abs(-42))
  assert_eq(0, Int::abs(0))
}

// 测试2: Math 模块基本运算
test "math_module_basic_operations" {
  // 测试 Math 模块的基本数学运算
  assert_eq(4.0, @double.round(3.7))
  assert_eq(4.0, @double.ceil(3.2))
  assert_eq(3.0, @double.floor(3.7))
  assert_eq(3.0, @double.trunc(3.7))
  assert_eq(-3.0, @double.trunc(-3.7))
}

// 测试3: String 模块基本功能
test "string_module_basic_functions" {
  // 测试 String 模块的基本功能
  assert_eq("", String::default())
  assert_eq("Hello, World!", "Hello, " + "World!")
  assert_eq(5, "Hello".length())
  assert_eq(true, "Hello".contains("ell"))
  assert_eq(false, "Hello".contains("world"))
}

// 测试4: 数值转换功能
test "numeric_conversion_functions" {
  // 测试数值转换功能
  assert_eq(42, "42".to_int())
  assert_eq(0, "".to_int())
  assert_eq(3.14, "3.14".to_double())
  assert_eq("42", 42.to_string())
  assert_eq("3.14", 3.14.to_string())
}

// 测试5: 数组操作功能
test "array_operations" {
  // 测试数组操作功能
  let arr = [1, 2, 3, 4, 5]
  assert_eq(5, arr.length())
  assert_eq(1, arr[0])
  assert_eq(5, arr[4])
  assert_eq(true, arr.contains(3))
  assert_eq(false, arr.contains(6))
}

// 测试6: 选项类型处理
test "option_type_handling" {
  // 测试选项类型的处理
  let some_value = Some(42)
  let none_value = None
  
  assert_eq(true, some_value.is_some())
  assert_eq(false, some_value.is_none())
  assert_eq(false, none_value.is_some())
  assert_eq(true, none_value.is_none())
  assert_eq(42, some_value.unwrap_or(0))
  assert_eq(0, none_value.unwrap_or(0))
}

// 测试7: 结果类型处理
test "result_type_handling" {
  // 测试结果类型的处理
  let ok_result = Ok(42)
  let err_result = Err("Error message")
  
  assert_eq(true, ok_result.is_ok())
  assert_eq(false, ok_result.is_err())
  assert_eq(false, err_result.is_ok())
  assert_eq(true, err_result.is_err())
  assert_eq(42, ok_result.unwrap_or(0))
  assert_eq(0, err_result.unwrap_or(0))
}

// 测试8: 字符串高级操作
test "string_advanced_operations" {
  // 测试字符串的高级操作
  let text = "Hello, World!"
  assert_eq("hello, world!", text.lowercase())
  assert_eq("HELLO, WORLD!", text.uppercase())
  assert_eq("Hello", text.split(",")[0])
  assert_eq(" World!", text.split(",")[1])
  assert_eq("Hello World!", text.replace(",", ""))
}

// 测试9: 数学函数高级应用
test "math_advanced_functions" {
  // 测试数学函数的高级应用
  let x = 16.0
  let y = 2.0
  
  assert_eq(4.0, @double.sqrt(x))
  assert_eq(256.0, @double.pow(x, y))
  assert_eq(10.0, @double.log10(100.0))
  assert_eq(2.718281828459045, @double.exp(1.0))
}

// 测试10: 综合业务场景测试
test "comprehensive_business_scenario" {
  // 测试综合业务场景：计算订单总价
  let prices = [10.5, 20.0, 15.75, 5.25]
  let quantities = [2, 1, 3, 4]
  
  // 计算每个商品的总价
  let item_totals = [
    prices[0] * quantities[0].to_double(),
    prices[1] * quantities[1].to_double(),
    prices[2] * quantities[2].to_double(),
    prices[3] * quantities[3].to_double()
  ]
  
  // 计算订单总价
  let subtotal = item_totals[0] + item_totals[1] + item_totals[2] + item_totals[3]
  let tax_rate = 0.08
  let tax = subtotal * tax_rate
  let total = subtotal + tax
  
  // 验证计算结果
  assert_eq(21.0, item_totals[0])  // 10.5 * 2
  assert_eq(20.0, item_totals[1])  // 20.0 * 1
  assert_eq(47.25, item_totals[2]) // 15.75 * 3
  assert_eq(21.0, item_totals[3])  // 5.25 * 4
  assert_eq(109.25, subtotal)      // 21.0 + 20.0 + 47.25 + 21.0
  assert_eq(8.74, @double.round(tax * 100.0) / 100.0)  // 109.25 * 0.08 = 8.74
  assert_eq(117.99, @double.round(total * 100.0) / 100.0) // 109.25 + 8.74 = 117.99
}