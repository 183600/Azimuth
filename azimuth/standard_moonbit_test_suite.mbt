// 标准 MoonBit 测试套件
// 包含针对 azimuth 库函数的高质量测试用例

test "add_function_comprehensive" {
  // 基本加法测试
  assert_eq(10, add(7, 3))
  // 零元素测试
  assert_eq(7, add(7, 0))
  assert_eq(7, add(0, 7))
  // 负数测试
  assert_eq(0, add(5, -5))
  assert_eq(-10, add(-7, -3))
  // 大数测试
  assert_eq(20000, add(10000, 10000))
}

test "multiply_function_comprehensive" {
  // 基本乘法测试
  assert_eq(21, multiply(7, 3))
  // 零元素测试
  assert_eq(0, multiply(7, 0))
  assert_eq(0, multiply(0, 7))
  // 负数测试
  assert_eq(-21, multiply(7, -3))
  assert_eq(21, multiply(-7, -3))
  // 单位元测试
  assert_eq(7, multiply(7, 1))
  assert_eq(7, multiply(1, 7))
}

test "greet_function_comprehensive" {
  // 基本问候测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  // 空字符串测试
  assert_eq_string("Hello, !", greet(""))
  // 特殊字符测试
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  // 长字符串测试
  let long_name = "VeryLongNameThatTestsStringHandling"
  assert_eq_string("Hello, " + long_name + "!", greet(long_name))
}

test "divide_with_ceil_comprehensive" {
  // 正数除法向上取整
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 3))
  
  // 负数除法向上取整
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(2, divide_with_ceil(3, 2))
}

test "mathematical_properties" {
  // 加法交换律
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律测试
  let c = 5
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "edge_case_calculations" {
  // 边界值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  
  // 大数计算
  let big_num = 1000000
  assert_eq(2000000, add(big_num, big_num))
  assert_eq(1000000000000, multiply(big_num, big_num))
  
  // 混合运算
  assert_eq(25, add(multiply(4, 5), divide_with_ceil(10, 3)))
}

test "error_handling" {
  // 除零处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "complex_scenarios" {
  // 包装计算场景
  let total_products = 47
  let products_per_package = 12
  let needed_packages = divide_with_ceil(total_products, products_per_package)
  assert_eq(4, needed_packages)
  
  // 价格计算场景
  let unit_price = 15
  let quantity = 8
  let subtotal = multiply(unit_price, quantity)
  let discount = 20
  let final_total = add(subtotal, -discount)
  assert_eq(100, final_total)
}

test "unicode_and_special_chars" {
  // Unicode 字符测试
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, 🌟🌙!", greet("🌟🌙"))
  
  // 特殊空白字符
  assert_eq_string("Hello, \t\n\r!", greet("\t\n\r"))
}

test "performance_considerations" {
  // 重复操作测试
  let mut result = 0
  result = add(result, 1)
  result = add(result, 2)
  result = add(result, 3)
  assert_eq(6, result)
  
  // 累积乘法
  let mut product = 1
  product = multiply(product, 2)
  product = multiply(product, 3)
  product = multiply(product, 4)
  assert_eq(24, product)
}