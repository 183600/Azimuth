// 高质量 MoonBit 标准测试用例
// 使用标准 MoonBit 测试语法，包含9个精心设计的测试用例
// 覆盖核心功能：数值运算、字符串处理、列表操作、条件逻辑等

test "int_precision_arithmetic" {
  // 精确整数算术运算测试
  let a = 123456789
  let b = 987654321
  
  // 大数加法
  assert_eq(a + b, 1111111110)
  
  // 大数乘法
  assert_eq(a * 1, a)
  assert_eq(a * 0, 0)
  
  // 除法精度测试
  assert_eq(10 / 3, 3)
  assert_eq(10 % 3, 1)
  
  // 负数运算
  assert_eq(-a + a, 0)
  assert_eq(a - a, 0)
}

test "string_basic_operations" {
  // 字符串基本操作测试
  let english = "Hello"
  let chinese = "你好"
  let mixed = "Hello世界"
  
  // 字符串连接
  assert_eq(english + chinese, "Hello你好")
  assert_eq(mixed, "Hello世界")
  
  // 字符串长度测试
  assert_eq(english.length(), 5)
  assert_eq(chinese.length(), 2)
  assert_eq(mixed.length(), 7)
  
  // 空字符串处理
  let empty = ""
  assert_eq(empty.length(), 0)
  assert_eq(empty + english, english)
}

test "list_comprehensive_operations" {
  // 列表综合操作测试
  let empty = []
  let single = [42]
  let numbers = [1, 2, 3, 4, 5]
  
  // 基本属性
  assert_eq(empty.length(), 0)
  assert_eq(single.length(), 1)
  assert_eq(numbers.length(), 5)
  
  assert_true(empty.is_empty())
  assert_false(single.is_empty())
  
  // 元素访问
  assert_eq(single[0], 42)
  assert_eq(numbers[0], 1)
  assert_eq(numbers[4], 5)
  
  // 列表连接
  let combined = numbers + [6, 7, 8]
  assert_eq(combined.length(), 8)
  assert_eq(combined[5], 6)
  assert_eq(combined[7], 8)
}

test "boolean_logic_truth_table" {
  // 布尔逻辑真值表测试
  let t = true
  let f = false
  
  // AND 运算
  assert_true(t && t)
  assert_false(t && f)
  assert_false(f && t)
  assert_false(f && f)
  
  // OR 运算
  assert_true(t || t)
  assert_true(t || f)
  assert_true(f || t)
  assert_false(f || f)
  
  // NOT 运算
  assert_false(!t)
  assert_true(!f)
  
  // 复合逻辑
  assert_true((t && f) || (t || f))
  assert_false((f && f) || (f && t))
}

test "char_encoding_and_conversion" {
  // 字符编码和转换测试
  let lowercase = 'a'
  let uppercase = 'A'
  let digit = '5'
  let space = ' '
  
  // 字符比较
  assert_true(lowercase != uppercase)
  assert_true(digit >= '0' && digit <= '9')
  assert_true(space == ' ')
  
  // 字符到整数转换
  assert_eq(lowercase.to_int(), 97)
  assert_eq(uppercase.to_int(), 65)
  assert_eq(digit.to_int(), 53)
  assert_eq(space.to_int(), 32)
  
  // 数字字符验证
  let is_digit = fn(c : Char) { c >= '0' && c <= '9' }
  assert_true(is_digit('0'))
  assert_true(is_digit('9'))
  assert_false(is_digit('a'))
  assert_true(is_digit(digit))
}

test "mathematical_calculations" {
  // 数学计算测试
  // 最大最小值（使用条件表达式模拟）
  let max_int = fn(a : Int, b : Int) { if a > b { a } else { b } }
  let min_int = fn(a : Int, b : Int) { if a < b { a } else { b } }
  let abs_int = fn(x : Int) { if x >= 0 { x } else { -x } }
  
  assert_eq(max_int(10, 20), 20)
  assert_eq(max_int(-5, -10), -5)
  assert_eq(min_int(10, 20), 10)
  assert_eq(min_int(-5, -10), -10)
  
  assert_eq(abs_int(42), 42)
  assert_eq(abs_int(-42), 42)
  assert_eq(abs_int(0), 0)
  
  // 幂运算模拟
  let power = fn(base : Int, exp : Int) {
    if exp == 0 { 1 } else { base * power(base, exp - 1) }
  }
  
  assert_eq(power(2, 3), 8)
  assert_eq(power(3, 2), 9)
  assert_eq(power(5, 0), 1)
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  // 场景1：购物车总价计算
  let prices = [29, 49, 99, 15]
  let quantities = [2, 1, 1, 3]
  let discount = 20
  let shipping = 10
  
  let subtotal = prices[0] * quantities[0] + prices[1] * quantities[1] + 
                 prices[2] * quantities[2] + prices[3] * quantities[3]
  let total = subtotal - discount + shipping
  
  assert_eq(subtotal, 251)  // 29*2 + 49*1 + 99*1 + 15*3 = 58 + 49 + 99 + 45 = 251
  assert_eq(total, 241)     // 251 - 20 + 10 = 241
  
  // 场景2：平均分计算
  let scores = [85, 92, 78, 96, 88]
  let sum = scores[0] + scores[1] + scores[2] + scores[3] + scores[4]
  let average = sum / scores.length()
  
  assert_eq(sum, 439)
  assert_eq(average, 87)
}

test "comparison_operations" {
  // 比较运算测试
  let a = 10
  let b = 20
  let c = 10
  
  // 小于大于
  assert_true(a < b)
  assert_false(a > b)
  assert_true(a <= b)
  assert_false(a >= b)
  
  // 等于不等于
  assert_false(a == b)
  assert_true(a != b)
  assert_true(a == c)
  assert_true(a <= c)
  assert_true(a >= c)
  
  // 字符串比较
  let str1 = "apple"
  let str2 = "banana"
  let str3 = "apple"
  
  assert_false(str1 == str2)
  assert_true(str1 == str3)
  assert_true(str1 != str2)
}

test "algorithm_implementation" {
  // 算法实现测试
  // 斐波那契数列
  let fibonacci = fn(n : Int) -> Int {
    if n <= 1 { n } else { fibonacci(n - 1) + fibonacci(n - 2) }
  }
  
  assert_eq(fibonacci(0), 0)
  assert_eq(fibonacci(1), 1)
  assert_eq(fibonacci(2), 1)
  assert_eq(fibonacci(3), 2)
  assert_eq(fibonacci(4), 3)
  assert_eq(fibonacci(5), 5)
  assert_eq(fibonacci(6), 8)
  
  // 最大公约数
  let gcd = fn(a : Int, b : Int) -> Int {
    if b == 0 { a } else { gcd(b, a % b) }
  }
  
  assert_eq(gcd(48, 18), 6)
  assert_eq(gcd(17, 13), 1)
  assert_eq(gcd(100, 25), 25)
  assert_eq(gcd(0, 5), 5)
}