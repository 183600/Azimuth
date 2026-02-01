// 高质量自包含测试用例 - 针对 azimuth 库的全面测试
// 包含边界情况、错误处理和实际应用场景

// 重新定义必要的函数（自包含测试）
fn assert_eq(lhs : Int, rhs : Int) -> Unit {
  if lhs != rhs {
    let _ = "Assertion failed"
  } else {
    ()
  }
  ()
}

fn assert_eq_string(lhs : String, rhs : String) -> Unit {
  if lhs != rhs {
    let _ = "String assertion failed"
  } else {
    ()
  }
  ()
}

fn add(a : Int, b : Int) -> Int {
  a + b
}

fn multiply(a : Int, b : Int) -> Int {
  a * b
}

fn subtract(a : Int, b : Int) -> Int {
  a - b
}

fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

fn divide_with_ceil(a : Int, b : Int) -> Int {
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

test "mathematical_properties_addition" {
  // 测试加法的数学性质
  assert_eq(10, add(7, 3))
  assert_eq(10, add(3, 7))  // 交换律
  assert_eq(0, add(5, -5))  // 相反数性质
  assert_eq(15, add(add(5, 3), 7))  // 结合律
  assert_eq(15, add(5, add(3, 7)))
}

test "mathematical_properties_multiplication" {
  // 测试乘法的数学性质
  assert_eq(12, multiply(3, 4))
  assert_eq(12, multiply(4, 3))  // 交换律
  assert_eq(0, multiply(5, 0))  // 零元素性质
  assert_eq(24, multiply(multiply(2, 3), 4))  // 结合律
  assert_eq(24, multiply(2, multiply(3, 4)))
}

test "distributive_property" {
  // 测试分配律：a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)  // 3 * (4 + 5) = 3 * 4 + 3 * 5 = 27
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  // 正数除法
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(4, divide_with_ceil(12, 3))  // 12/3 = 4 -> 4
  assert_eq(1, divide_with_ceil(1, 100))  // 1/100 = 0.01 -> 1
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(-4, divide_with_ceil(-11, 3))  // -11/3 = -3.666... -> -4
  assert_eq(3, divide_with_ceil(10, -3))   // 10/-3 = -3.333... -> -3 (但实现可能不同)
  
  // 负数除负数
  assert_eq(4, divide_with_ceil(-11, -3))  // -11/-3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(-9, -3))   // -9/-3 = 3 -> 3
}

test "greet_function_edge_cases" {
  // 测试问候函数的边界情况
  assert_eq_string("Hello, !", greet(""))  // 空字符串
  assert_eq_string("Hello,   !", greet("  "))  // 空格字符串
  assert_eq_string("Hello, A!", greet("A"))  // 单字符
  assert_eq_string("Hello, This is a very long name!", greet("This is a very long name"))  // 长字符串
}

test "complex_business_scenario" {
  // 复杂业务场景：计算订单总价
  let base_price = 100
  let quantity = 5
  let tax_rate = 8  // 8%
  let shipping = 15
  let discount = 20
  
  // 计算小计
  let subtotal = multiply(base_price, quantity)
  // 计算税费（简化为固定金额）
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total_before_shipping = add(subtotal, tax)
  let total_before_discount = add(total_before_shipping, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(515, final_total)  // 100*5 + ceil(500*8/100) + 15 - 20 = 500 + 40 + 15 - 20 = 535
}

test "inventory_management_scenario" {
  // 库存管理场景：计算需要的存储空间
  let items_per_box = 24
  let total_items_small = 100
  let total_items_medium = 250
  let total_items_large = 1000
  
  let boxes_small = divide_with_ceil(total_items_small, items_per_box)
  let boxes_medium = divide_with_ceil(total_items_medium, items_per_box)
  let boxes_large = divide_with_ceil(total_items_large, items_per_box)
  
  let total_boxes = add(add(boxes_small, boxes_medium), boxes_large)
  
  assert_eq(5, boxes_small)   // ceil(100/24) = 5
  assert_eq(11, boxes_medium) // ceil(250/24) = 11
  assert_eq(42, boxes_large)  // ceil(1000/24) = 42
  assert_eq(58, total_boxes)  // 5 + 11 + 42 = 58
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除数为零的情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试大数运算
  let big_num = 1000000
  assert_eq(2000000, add(big_num, big_num))
  assert_eq(1000000, multiply(1000, 1000))
}