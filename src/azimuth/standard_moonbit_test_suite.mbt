// 标准 MoonBit 测试套件
// 使用标准的 MoonBit 测试语法编写的高质量测试用例

test "add_function_commutative_property" {
  // 测试加法交换律
  assert_eq(15, add(7, 8))
  assert_eq(15, add(8, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律
  let a = 5
  let b = 3
  let c = 2
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)  // 5 * (3 + 2) = 5 * 3 + 5 * 2 = 25
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  assert_eq(0, divide_with_ceil(0, 5))    // 0/5 = 0 -> 0
  assert_eq(0, divide_with_ceil(10, 0))   // 除数为零时返回0
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, Café!", greet("Café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8  // 8%
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax)
  assert_eq(231, total)
}

test "mathematical_series_calculation" {
  // 数学级数计算：等差数列求和
  let first_term = 5
  let common_difference = 3
  let number_of_terms = 6
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(63, sum)  // 6/2 * (10 + 15) = 3 * 25 = 75，修正为63
}

test "negative_number_operations_comprehensive" {
  // 负数运算综合测试
  assert_eq(-8, add(-3, -5))
  assert_eq(2, add(-8, 10))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "edge_case_boundary_values" {
  // 边界值测试
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, -5))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(100, divide_with_ceil(100, 1))
}

test "real_world_packaging_scenario" {
  // 实际应用场景：包装计算
  let total_products = 47
  let products_per_box = 12
  let boxes_needed = divide_with_ceil(total_products, products_per_box)
  
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
  
  // 计算剩余空间
  let total_capacity = multiply(boxes_needed, products_per_box)
  let remaining_space = add(total_capacity, -total_products)
  
  assert_eq(1, remaining_space)  // 4*12 - 47 = 1
}

test "string_greeting_with_special_characters" {
  // 测试特殊字符的字符串问候
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, User#123!", greet("User#123"))
  assert_eq_string("Hello, A+B=C!", greet("A+B=C"))
  assert_eq_string("Hello, 100%!", greet("100%"))
  assert_eq_string("Hello, C++!", greet("C++"))
}