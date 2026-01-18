// Azimuth 项目增强测试套件
// 使用标准 MoonBit 测试语法，包含多样化的测试场景

test "array_operations" {
  // 数组操作测试
  let numbers = [1, 2, 3, 4, 5]
  let first = numbers[0]
  let last = numbers[4]
  let middle = numbers[2]
  
  @azimuth.assert_eq(1, first)
  @azimuth.assert_eq(5, last)
  @azimuth.assert_eq(3, middle)
  
  // 测试数组长度
  let length = numbers.length()
  @azimuth.assert_eq(5, length)
}

test "boolean_logic" {
  // 布尔逻辑测试
  let true_val = true
  let false_val = false
  
  // 测试逻辑与
  let and1 = true_val && true_val
  let and2 = true_val && false_val
  let and3 = false_val && true_val
  let and4 = false_val && false_val
  
  @azimuth.assert_true(and1)
  @azimuth.assert_false(and2)
  @azimuth.assert_false(and3)
  @azimuth.assert_false(and4)
  
  // 测试逻辑或
  let or1 = true_val || true_val
  let or2 = true_val || false_val
  let or3 = false_val || true_val
  let or4 = false_val || false_val
  
  @azimuth.assert_true(or1)
  @azimuth.assert_true(or2)
  @azimuth.assert_true(or3)
  @azimuth.assert_false(or4)
}

test "conditional_expressions" {
  // 条件表达式测试
  let x = 10
  let y = 20
  
  // 测试三元条件表达式
  let max1 = if x > y { x } else { y }
  let max2 = if y > x { y } else { x }
  let min1 = if x < y { x } else { y }
  
  @azimuth.assert_eq(20, max1)
  @azimuth.assert_eq(20, max2)
  @azimuth.assert_eq(10, min1)
  
  // 测试嵌套条件
  let result = if x > 5 {
    if y > 15 { "both high" } else { "x high" }
  } else {
    if y > 15 { "y high" } else { "both low" }
  }
  
  @azimuth.assert_eq_string("both high", result)
}

test "string_manipulation" {
  // 字符串操作测试
  let first_name = "John"
  let last_name = "Doe"
  let age = "30"
  
  // 测试字符串连接
  let full_name = first_name + " " + last_name
  let introduction = "My name is " + full_name + " and I am " + age + " years old"
  
  @azimuth.assert_eq_string("John Doe", full_name)
  @azimuth.assert_eq_string("My name is John Doe and I am 30 years old", introduction)
  
  // 测试字符串长度
  let name_length = full_name.length()
  @azimuth.assert_eq(8, name_length)
}

test "modulo_operations" {
  // 取模运算测试
  let a = 17
  let b = 5
  
  // 测试正数取模
  let mod1 = a % b
  @azimuth.assert_eq(2, mod1)
  
  // 测试边界情况
  let mod2 = 10 % 1
  @azimuth.assert_eq(0, mod2)
  
  let mod3 = 10 % 10
  @azimuth.assert_eq(0, mod3)
  
  // 测试负数取模
  let mod4 = -17 % 5
  let mod5 = 17 % -5
  
  // 结果可能因语言而异，这里只验证基本行为
  let abs_mod4 = if mod4 < 0 { -mod4 } else { mod4 }
  let abs_mod5 = if mod5 < 0 { -mod5 } else { mod5 }
  
  @azimuth.assert_eq(2, abs_mod4)
  @azimuth.assert_eq(2, abs_mod5)
}

test "power_calculations" {
  // 幂运算测试（使用乘法模拟）
  let base = 3
  
  // 计算 3^2 = 9
  let power2 = base * base
  @azimuth.assert_eq(9, power2)
  
  // 计算 3^3 = 27
  let power3 = base * base * base
  @azimuth.assert_eq(27, power3)
  
  // 计算 3^4 = 81
  let power4 = base * base * base * base
  @azimuth.assert_eq(81, power4)
  
  // 测试特殊情况
  let zero_power = 1  // 任何数的0次方都是1
  let one_power = base  // 任何数的1次方都是它本身
  
  @azimuth.assert_eq(1, zero_power)
  @azimuth.assert_eq(3, one_power)
}

test "factorial_calculations" {
  // 阶乘计算测试（使用循环展开）
  // 5! = 5 × 4 × 3 × 2 × 1 = 120
  let factorial5 = 5 * 4 * 3 * 2 * 1
  @azimuth.assert_eq(120, factorial5)
  
  // 4! = 4 × 3 × 2 × 1 = 24
  let factorial4 = 4 * 3 * 2 * 1
  @azimuth.assert_eq(24, factorial4)
  
  // 3! = 3 × 2 × 1 = 6
  let factorial3 = 3 * 2 * 1
  @azimuth.assert_eq(6, factorial3)
  
  // 2! = 2 × 1 = 2
  let factorial2 = 2 * 1
  @azimuth.assert_eq(2, factorial2)
  
  // 1! = 1
  let factorial1 = 1
  @azimuth.assert_eq(1, factorial1)
  
  // 0! = 1（数学定义）
  let factorial0 = 1
  @azimuth.assert_eq(1, factorial0)
}

test "fibonacci_sequence" {
  // 斐波那契数列测试
  let fib0 = 0
  let fib1 = 1
  let fib2 = fib0 + fib1  // 1
  let fib3 = fib1 + fib2  // 2
  let fib4 = fib2 + fib3  // 3
  let fib5 = fib3 + fib4  // 5
  let fib6 = fib4 + fib5  // 8
  let fib7 = fib5 + fib6  // 13
  let fib8 = fib6 + fib7  // 21
  let fib9 = fib7 + fib8  // 34
  
  @azimuth.assert_eq(0, fib0)
  @azimuth.assert_eq(1, fib1)
  @azimuth.assert_eq(1, fib2)
  @azimuth.assert_eq(2, fib3)
  @azimuth.assert_eq(3, fib4)
  @azimuth.assert_eq(5, fib5)
  @azimuth.assert_eq(8, fib6)
  @azimuth.assert_eq(13, fib7)
  @azimuth.assert_eq(21, fib8)
  @azimuth.assert_eq(34, fib9)
}

test "greatest_common_divisor" {
  // 最大公约数测试（欧几里得算法思路）
  // gcd(48, 18) = 6
  let a = 48
  let b = 18
  
  // 手动模拟欧几里得算法
  let step1 = a % b  // 48 % 18 = 12
  let step2 = b % step1  // 18 % 12 = 6
  let step3 = step1 % step2  // 12 % 6 = 0
  
  @azimuth.assert_eq(12, step1)
  @azimuth.assert_eq(6, step2)
  @azimuth.assert_eq(0, step3)
  
  // 当余数为0时，除数就是最大公约数
  let gcd = step2
  @azimuth.assert_eq(6, gcd)
  
  // 测试另一对数字
  let c = 56
  let d = 98
  
  let step4 = c % d  // 56 % 98 = 56
  let step5 = d % step4  // 98 % 56 = 42
  let step6 = step4 % step5  // 56 % 42 = 14
  let step7 = step5 % step6  // 42 % 14 = 0
  
  @azimuth.assert_eq(56, step4)
  @azimuth.assert_eq(42, step5)
  @azimuth.assert_eq(14, step6)
  @azimuth.assert_eq(0, step7)
  
  let gcd2 = step6
  @azimuth.assert_eq(14, gcd2)
}

test "prime_number_check" {
  // 质数检查测试
  // 检查7是否为质数
  let num = 7
  let is_prime_7 = true
  
  // 7不能被2整除
  let check1 = 7 % 2
  @azimuth.assert_eq(1, check1)
  
  // 7不能被3整除
  let check2 = 7 % 3
  @azimuth.assert_eq(1, check2)
  
  // 7不能被4、5、6整除
  let check3 = 7 % 4
  let check4 = 7 % 5
  let check5 = 7 % 6
  
  @azimuth.assert_eq(3, check3)
  @azimuth.assert_eq(2, check4)
  @azimuth.assert_eq(1, check5)
  
  // 检查9是否为质数（应该不是）
  let num2 = 9
  let check6 = 9 % 3
  @azimuth.assert_eq(0, check6)
  
  // 检查11是否为质数
  let check7 = 11 % 2
  let check8 = 11 % 3
  let check9 = 11 % 5
  let check10 = 11 % 7
  
  @azimuth.assert_eq(1, check7)
  @azimuth.assert_eq(2, check8)
  @azimuth.assert_eq(1, check9)
  @azimuth.assert_eq(4, check10)
}