// 高质量 MoonBit 测试套件 - 简化版
// 针对 azimuth 库的核心测试，使用内置断言方法

test "basic_math_operations" {
  // 测试基本数学运算
  let add_result = @azimuth.add(5, 3)
  if (add_result != 8) {
    @builtin.abort("Test failed: basic addition")
  }
  
  let multiply_result = @azimuth.multiply(4, 6)
  if (multiply_result != 24) {
    @builtin.abort("Test failed: basic multiplication")
  }
  
  let divide_result = @azimuth.divide_with_ceil(10, 3)
  if (divide_result != 4) {
    @builtin.abort("Test failed: ceiling division")
  }
}

test "greet_functionality" {
  // 测试问候功能
  let greet_result = @azimuth.greet("MoonBit")
  if (greet_result != "Hello, MoonBit!") {
    @builtin.abort("Test failed: greet functionality")
  }
  
  let empty_greet = @azimuth.greet("")
  if (empty_greet != "Hello, !") {
    @builtin.abort("Test failed: empty greet")
  }
}

test "negative_number_operations" {
  // 测试负数运算
  let neg_add = @azimuth.add(-5, -3)
  if (neg_add != -8) {
    @builtin.abort("Test failed: negative addition")
  }
  
  let neg_multiply = @azimuth.multiply(-4, 5)
  if (neg_multiply != -20) {
    @builtin.abort("Test failed: negative multiplication")
  }
  
  let neg_divide = @azimuth.divide_with_ceil(-10, 3)
  if (neg_divide != -3) {
    @builtin.abort("Test failed: negative division")
  }
}

test "business_calculation" {
  // 测试业务计算场景
  let price = 25
  let quantity = 8
  let shipping = 15
  
  let subtotal = @azimuth.multiply(price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  
  if (subtotal != 200) {
    @builtin.abort("Test failed: business subtotal")
  }
  
  if (total != 215) {
    @builtin.abort("Test failed: business total")
  }
}

test "packaging_calculation" {
  // 测试包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  
  if (boxes_needed != 5) {
    @builtin.abort("Test failed: packaging calculation")
  }
}

test "unicode_support" {
  // 测试Unicode支持
  let unicode_greet = @azimuth.greet("世界")
  if (unicode_greet != "Hello, 世界!") {
    @builtin.abort("Test failed: unicode support")
  }
  
  let emoji_greet = @azimuth.greet("🚀")
  if (emoji_greet != "Hello, 🚀!") {
    @builtin.abort("Test failed: emoji support")
  }
}

test "edge_case_division" {
  // 测试除法边界情况
  let zero_divisor = @azimuth.divide_with_ceil(100, 0)
  if (zero_divisor != 0) {
    @builtin.abort("Test failed: zero division")
  }
  
  let exact_division = @azimuth.divide_with_ceil(12, 3)
  if (exact_division != 4) {
    @builtin.abort("Test failed: exact division")
  }
  
  let small_fraction = @azimuth.divide_with_ceil(1, 100)
  if (small_fraction != 1) {
    @builtin.abort("Test failed: small fraction")
  }
}

test "complex_calculation" {
  // 测试复杂计算
  let result1 = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  if (result1 != 26) {  // 2*3 + 4*5 = 6 + 20 = 26
    @builtin.abort("Test failed: complex calculation 1")
  }
  
  let result2 = @azimuth.multiply(@azimuth.add(5, 3), 2)
  if (result2 != 16) {  // (5 + 3) * 2 = 8 * 2 = 16
    @builtin.abort("Test failed: complex calculation 2")
  }
}

test "large_number_operations" {
  // 测试大数运算
  let large_add = @azimuth.add(1000000, 1000000)
  if (large_add != 2000000) {
    @builtin.abort("Test failed: large number addition")
  }
  
  let large_multiply = @azimuth.multiply(1000, 1000)
  if (large_multiply != 1000000) {
    @builtin.abort("Test failed: large number multiplication")
  }
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  let commutative_add1 = @azimuth.add(a, b)
  let commutative_add2 = @azimuth.add(b, a)
  if (commutative_add1 != commutative_add2) {
    @builtin.abort("Test failed: addition commutative property")
  }
  
  // 乘法交换律
  let commutative_mul1 = @azimuth.multiply(a, b)
  let commutative_mul2 = @azimuth.multiply(b, a)
  if (commutative_mul1 != commutative_mul2) {
    @builtin.abort("Test failed: multiplication commutative property")
  }
  
  // 零元素性质
  let zero_mul = @azimuth.multiply(a, 0)
  if (zero_mul != 0) {
    @builtin.abort("Test failed: zero element property")
  }
  
  let zero_add = @azimuth.add(a, 0)
  if (zero_add != a) {
    @builtin.abort("Test failed: zero additive property")
  }
}