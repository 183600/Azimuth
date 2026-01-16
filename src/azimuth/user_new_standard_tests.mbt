// 用户新增的标准 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "user_add_basic" {
  // 基本加法测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "user_multiply_basic" {
  // 基本乘法测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(25, multiply(5, 5))
}

test "user_greet_basic" {
  // 基本问候函数测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "user_math_commutativity" {
  // 数学交换律验证
  let a = 15
  let b = 25
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "user_math_identities" {
  // 数学恒等式验证
  let x = 42
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
}

test "user_negative_operations" {
  // 负数运算测试
  let positive = 10
  let negative = -5
  
  // 正负数加法
  assert_eq(5, add(positive, negative))
  assert_eq(-15, add(negative, -positive))
  
  // 负数乘法
  assert_eq(-50, multiply(positive, negative))
  assert_eq(50, multiply(negative, -positive))
}

test "user_calculation_scenarios" {
  // 实际计算场景测试
  
  // 计算矩形面积
  let length = 8
  let width = 6
  let area = multiply(length, width)
  assert_eq(48, area)
  
  // 计算周长
  let perimeter = multiply(add(length, width), 2)
  assert_eq(28, perimeter)
}

test "user_string_operations" {
  // 字符串操作测试
  let name1 = "Alice"
  let name2 = "Bob"
  let empty = ""
  
  // 基本问候
  assert_eq_string("Hello, Alice!", greet(name1))
  assert_eq_string("Hello, Bob!", greet(name2))
  assert_eq_string("Hello, !", greet(empty))
  
  // 包含空格的名称
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
}