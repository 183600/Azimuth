// 标准 MoonBit 测试用例 - 为 clean_test 项目添加
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例

test "add_basic_functionality" {
  // 基本加法功能测试
  @clean_test.assert_eq(5, @clean_test.add(2, 3))
  @clean_test.assert_eq(0, @clean_test.add(0, 0))
  @clean_test.assert_eq(-5, @clean_test.add(-2, -3))
  @clean_test.assert_eq(10, @clean_test.add(7, 3))
}

test "multiply_basic_functionality" {
  // 基本乘法功能测试
  @clean_test.assert_eq(6, @clean_test.multiply(2, 3))
  @clean_test.assert_eq(0, @clean_test.multiply(0, 5))
  @clean_test.assert_eq(-6, @clean_test.multiply(-2, 3))
  @clean_test.assert_eq(15, @clean_test.multiply(3, 5))
}

test "greeting_functionality" {
  // 问候功能测试
  @clean_test.assert_eq_string("Hello, World!", @clean_test.greet("World"))
  @clean_test.assert_eq_string("Hello, MoonBit!", @clean_test.greet("MoonBit"))
  @clean_test.assert_eq_string("Hello, !", @clean_test.greet(""))
}

test "zero_value_operations" {
  // 零值运算测试
  @clean_test.assert_eq(0, @clean_test.multiply(5, 0))
  @clean_test.assert_eq(0, @clean_test.multiply(0, 0))
  @clean_test.assert_eq(5, @clean_test.add(5, 0))
}

test "large_number_operations" {
  // 大数运算测试
  @clean_test.assert_eq(2000000, @clean_test.add(1000000, 1000000))
  @clean_test.assert_eq(1000000, @clean_test.multiply(1000, 1000))
  @clean_test.assert_eq(10000, @clean_test.multiply(100, 100))
}

test "combined_operations" {
  // 组合运算测试
  let result1 = @clean_test.add(@clean_test.multiply(2, 3), @clean_test.multiply(4, 5))
  @clean_test.assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = @clean_test.add(@clean_test.multiply(2, 2), @clean_test.multiply(3, 3))
  @clean_test.assert_eq(13, result2)  // 2*2 + 3*3 = 4 + 9 = 13
}

test "mathematical_commutative_property" {
  // 数学交换律性质测试
  let a = 7
  let b = 13
  
  // 加法交换律
  @clean_test.assert_eq(@clean_test.add(a, b), @clean_test.add(b, a))
  
  // 乘法交换律
  @clean_test.assert_eq(@clean_test.multiply(a, b), @clean_test.multiply(b, a))
}

test "special_characters_handling" {
  // 特殊字符处理测试
  @clean_test.assert_eq_string("Hello, 世界!", @clean_test.greet("世界"))
  @clean_test.assert_eq_string("Hello, 🚀!", @clean_test.greet("🚀"))
  @clean_test.assert_eq_string("Hello, 123!", @clean_test.greet("123"))
  @clean_test.assert_eq_string("Hello, !@#$!", @clean_test.greet("!@#$"))
}