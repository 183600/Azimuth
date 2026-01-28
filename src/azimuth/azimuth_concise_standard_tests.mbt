// Azimuth 库的简洁标准测试用例
// 使用标准的 MoonBit 测试语法

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 7))
  assert_eq(-12, multiply(-3, 4))
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_positive" {
  // 测试正数的向上取整除法
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(1, divide_with_ceil(2, 3))
}

test "divide_with_ceil_negative" {
  // 测试负数的向上取整除法
  assert_eq(-2, divide_with_ceil(-8, 3))
  assert_eq(3, divide_with_ceil(-8, -3))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "zero_operations" {
  // 测试零相关运算
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "combined_operations" {
  // 测试组合运算
  let result = add(multiply(2, 4), multiply(3, 3))
  assert_eq(17, result)  // 2*4 + 3*3 = 8 + 9 = 17
}

test "practical_packaging" {
  // 实际应用场景：包装计算
  let items = 17
  let capacity = 5
  let boxes = divide_with_ceil(items, capacity)
  assert_eq(4, boxes)  // ceil(17/5) = 4
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 11
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
}

test "unicode_greeting" {
  // 测试Unicode字符问候
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}