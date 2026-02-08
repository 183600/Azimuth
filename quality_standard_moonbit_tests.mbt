// 高质量 MoonBit 标准测试用例
// 使用标准 MoonBit 测试语法，包含9个精心设计的测试用例
// 覆盖核心功能：数值运算、字符串处理、列表操作、条件逻辑等

test "int_precision_arithmetic" {
  // 精确整数算术运算测试
  let a = 123456789
  let b = 987654321
  
  // 大数加法
  @test.assert_eq(a + b, 1111111110)
  
  // 大数乘法
  @test.assert_eq(a * 1, a)
  @test.assert_eq(a * 0, 0)
  
  // 除法精度测试
  @test.assert_eq(10 / 3, 3)
  @test.assert_eq(10 % 3, 1)
  
  // 负数运算
  @test.assert_eq(-a + a, 0)
  @test.assert_eq(a - a, 0)
}

test "string_unicode_handling" {
  // Unicode 字符串处理测试
  let english = "Hello"
  let chinese = "你好"
  let mixed = "Hello世界"
  
  // 字符串连接
  @test.assert_eq(english + chinese, "Hello你好")
  @test.assert_eq(mixed, "Hello世界")
  
  // 字符串长度测试
  @test.assert_eq(english.length(), 5)
  @test.assert_eq(chinese.length(), 2)
  @test.assert_eq(mixed.length(), 7)
  
  // 空字符串处理
  let empty = ""
  @test.assert_eq(empty.length(), 0)
  @test.assert_eq(empty + english, english)
}

test "list_comprehensive_operations" {
  // 列表综合操作测试
  let empty = []
  let single = [42]
  let numbers = [1, 2, 3, 4, 5]
  
  // 基本属性
  @test.assert_eq(empty.length(), 0)
  @test.assert_eq(single.length(), 1)
  @test.assert_eq(numbers.length(), 5)
  
  @test.assert_true(empty.is_empty())
  @test.assert_false(single.is_empty())
  
  // 元素访问
  @test.assert_eq(single[0], 42)
  @test.assert_eq(numbers[0], 1)
  @test.assert_eq(numbers[4], 5)
  
  // 列表连接
  let combined = numbers + [6, 7, 8]
  @test.assert_eq(combined.length(), 8)
  @test.assert_eq(combined[5], 6)
  @test.assert_eq(combined[7], 8)
}

test "boolean_logic_truth_table" {
  // 布尔逻辑真值表测试
  let t = true
  let f = false
  
  // AND 运算
  @test.assert_true(t && t)
  @test.assert_false(t && f)
  @test.assert_false(f && t)
  @test.assert_false(f && f)
  
  // OR 运算
  @test.assert_true(t || t)
  @test.assert_true(t || f)
  @test.assert_true(f || t)
  @test.assert_false(f || f)
  
  // NOT 运算
  @test.assert_false(!t)
  @test.assert_true(!f)
  
  // 复合逻辑
  @test.assert_true((t && f) || (t || f))
  @test.assert_false((f && f) || (f && t))
}

test "char_encoding_and_conversion" {
  // 字符编码和转换测试
  let lowercase = 'a'
  let uppercase = 'A'
  let digit = '5'
  let space = ' '
  
  // 字符比较
  @test.assert_true(lowercase != uppercase)
  @test.assert_true(digit >= '0' && digit <= '9')
  @test.assert_true(space == ' ')
  
  // 字符到整数转换
  @test.assert_eq(lowercase.to_int(), 97)
  @test.assert_eq(uppercase.to_int(), 65)
  @test.assert_eq(digit.to_int(), 53)
  @test.assert_eq(space.to_int(), 32)
  
  // 数字字符验证
  let is_digit = fn(c : Char) { c >= '0' && c <= '9' }
  @test.assert_true(is_digit('0'))
  @test.assert_true(is_digit('9'))
  @test.assert_false(is_digit('a'))
  @test.assert_true(is_digit(digit))
}

test "option_type_operations" {
  // Option 类型操作测试
  let some_value = @test.Some(42)
  let none_value = @test.None
  
  // 值检查
  @test.assert_true(@test.is_some(some_value))
  @test.assert_false(@test.is_some(none_value))
  
  // 值提取
  @test.assert_eq(@test.unwrap(some_value), 42)
  
  // 默认值处理
  @test.assert_eq(@test.unwrap_or(some_value, 0), 42)
  @test.assert_eq(@test.unwrap_or(none_value, 0), 0)
}

test "mathematical_functions" {
  // 数学函数测试
  
  // 最大最小值
  @test.assert_eq(@test.max_int(10, 20), 20)
  @test.assert_eq(@test.max_int(-5, -10), -5)
  @test.assert_eq(@test.min_int(10, 20), 10)
  @test.assert_eq(@test.min_int(-5, -10), -10)
  
  // 绝对值
  @test.assert_eq(@test.abs_int(42), 42)
  @test.assert_eq(@test.abs_int(-42), 42)
  @test.assert_eq(@test.abs_int(0), 0)
  
  // 幂运算模拟
  let power = fn(base : Int, exp : Int) {
    if exp == 0 { 1 } else { base * power(base, exp - 1) }
  }
  
  @test.assert_eq(power(2, 3), 8)
  @test.assert_eq(power(3, 2), 9)
  @test.assert_eq(power(5, 0), 1)
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  // 场景1：购物车总价计算
  let prices = [29, 49, 99, 15]
  let quantities = [2, 1, 1, 3]
  let discount = 20
  let shipping = 10
  
  let subtotal = 0
  let i = 0
  while i < prices.length() {
    subtotal = subtotal + prices[i] * quantities[i]
    i = i + 1
  }
  
  let total = subtotal - discount + shipping
  
  @test.assert_eq(subtotal, 176)  // 29*2 + 49*1 + 99*1 + 15*3 = 58 + 49 + 99 + 45 = 251
  @test.assert_eq(total, 241)     // 251 - 20 + 10 = 241
  
  // 场景2：平均分计算
  let scores = [85, 92, 78, 96, 88]
  let sum = 0
  let j = 0
  while j < scores.length() {
    sum = sum + scores[j]
    j = j + 1
  }
  let average = sum / scores.length()
  
  @test.assert_eq(sum, 439)
  @test.assert_eq(average, 87)
}

test "algorithm_implementation" {
  // 算法实现测试
  // 斐波那契数列
  let fibonacci = fn(n : Int) -> Int {
    if n <= 1 { n } else { fibonacci(n - 1) + fibonacci(n - 2) }
  }
  
  @test.assert_eq(fibonacci(0), 0)
  @test.assert_eq(fibonacci(1), 1)
  @test.assert_eq(fibonacci(2), 1)
  @test.assert_eq(fibonacci(3), 2)
  @test.assert_eq(fibonacci(4), 3)
  @test.assert_eq(fibonacci(5), 5)
  @test.assert_eq(fibonacci(6), 8)
  
  // 最大公约数
  let gcd = fn(a : Int, b : Int) -> Int {
    if b == 0 { a } else { gcd(b, a % b) }
  }
  
  @test.assert_eq(gcd(48, 18), 6)
  @test.assert_eq(gcd(17, 13), 1)
  @test.assert_eq(gcd(100, 25), 25)
  @test.assert_eq(gcd(0, 5), 5)
}