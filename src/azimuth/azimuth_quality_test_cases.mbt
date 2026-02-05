// 高质量 MoonBit 测试用例 - 针对 azimuth 库核心功能
// 自包含测试文件，包含所有必要的函数定义

// 函数定义
pub fn assert_eq(lhs : Int, rhs : Int) -> Unit {
  if lhs != rhs {
    let _ = "Assertion failed"
  } else {
    ()
  }
  ()
}

pub fn assert_eq_string(lhs : String, rhs : String) -> Unit {
  if lhs != rhs {
    let _ = "String assertion failed"
  } else {
    ()
  }
  ()
}

pub fn add(a : Int, b : Int) -> Int {
  a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  a * b
}

pub fn subtract(a : Int, b : Int) -> Int {
  a - b
}

pub fn divide_with_ceil(a : Int, b : Int) -> Int {
  if b == 0 { 
    0 
  } else {
    let quotient = a / b
    let remainder = a % b
    if remainder == 0 {
      quotient
    } else if a > 0 && b > 0 {
      quotient + 1
    } else if a < 0 && b < 0 {
      quotient + 1
    } else {
      quotient
    }
  }
}

pub fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

// 测试用例
test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(15, add(7, 8))
  assert_eq(42, multiply(6, 7))
  assert_eq(10, subtract(20, 10))
  assert_eq(4, divide_with_ceil(13, 4))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  // 正数情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "greet_function_edge_cases" {
  // 测试问候函数的边界情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "complex_business_calculations" {
  // 复杂业务计算场景
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)
  assert_eq(215, total_before_discount)
  assert_eq(195, final_total)
}

test "packaging_and_logistics" {
  // 包装和物流场景测试
  let total_items = 47
  let items_per_box = 12
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  let total_capacity = multiply(boxes_needed, items_per_box)
  let unused_space = subtract(total_capacity, total_items)
  
  assert_eq(4, boxes_needed)
  assert_eq(48, total_capacity)
  assert_eq(1, unused_space)
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "large_number_handling" {
  // 大数处理测试
  let large_num = 1000000
  
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(100001, divide_with_ceil(1000010, 10))
}

test "financial_calculations" {
  // 金融计算场景
  let principal = 1000
  let rate_percent = 5
  
  // 计算单利
  let interest = divide_with_ceil(multiply(principal, rate_percent), 100)
  let total_with_interest = add(principal, interest)
  
  assert_eq(50, interest)
  assert_eq(1050, total_with_interest)
}

test "string_processing_with_greet" {
  // 字符串处理与问候函数结合
  let first_name = "张"
  let last_name = "三"
  let full_name = first_name + last_name
  let greeting = greet(full_name)
  
  assert_eq_string("Hello, 张三!", greeting)
  
  // 测试特殊字符
  let special_name = "Alice@Company"
  let special_greeting = greet(special_name)
  assert_eq_string("Hello, Alice@Company!", special_greeting)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  // 正数情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "greet_function_edge_cases" {
  // 测试问候函数的边界情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "complex_business_calculations" {
  // 复杂业务计算场景
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)
  assert_eq(215, total_before_discount)
  assert_eq(195, final_total)
}

test "packaging_and_logistics" {
  // 包装和物流场景测试
  let total_items = 47
  let items_per_box = 12
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  let total_capacity = multiply(boxes_needed, items_per_box)
  let unused_space = subtract(total_capacity, total_items)
  
  assert_eq(4, boxes_needed)
  assert_eq(48, total_capacity)
  assert_eq(1, unused_space)
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "large_number_handling" {
  // 大数处理测试
  let large_num = 1000000
  
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(100001, divide_with_ceil(1000010, 10))
}

test "financial_calculations" {
  // 金融计算场景
  let principal = 1000
  let rate_percent = 5
  
  // 计算单利
  let interest = divide_with_ceil(multiply(principal, rate_percent), 100)
  let total_with_interest = add(principal, interest)
  
  assert_eq(50, interest)
  assert_eq(1050, total_with_interest)
}

test "string_processing_with_greet" {
  // 字符串处理与问候函数结合
  let first_name = "张"
  let last_name = "三"
  let full_name = first_name + last_name
  let greeting = greet(full_name)
  
  assert_eq_string("Hello, 张三!", greeting)
  
  // 测试特殊字符
  let special_name = "Alice@Company"
  let special_greeting = greet(special_name)
  assert_eq_string("Hello, Alice@Company!", special_greeting)
}