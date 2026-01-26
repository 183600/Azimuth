// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含10个简洁而全面的测试用例

test "add_function_basic" {
  // 测试加法函数的基本功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(7, 3))
}

test "multiply_function_basic" {
  // 测试乘法函数的基本功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(3, 5))
}

test "greet_function_basic" {
  // 测试问候函数的基本功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法函数的基本功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "combined_operations" {
  // 测试组合运算
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = multiply(add(5, 3), 2)
  assert_eq(16, result2)  // (5 + 3) * 2 = 8 * 2 = 16
}

test "boundary_values" {
  // 测试边界值
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(0, multiply(1, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1000000, add(500000, 500000))
}

test "negative_operations" {
  // 测试负数运算
  assert_eq(-5, add(-2, -3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "special_characters" {
  // 测试特殊字符处理
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
}

test "practical_scenarios" {
  // 测试实际应用场景
  // 包装计算：25个物品，每箱6个，需要多少箱
  let boxes_needed = divide_with_ceil(25, 6)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 购物车计算：商品价格×数量+运费
  let total = add(multiply(25, 4), 15)
  assert_eq(115, total)  // 25*4 + 15 = 100 + 15 = 115
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
  
  // 分配律：a * (b + c) = a * b + a * c
  let c = 5
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)  // 7 * (13 + 5) = 7 * 13 + 7 * 5 = 126
}