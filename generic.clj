(ns com.shudi.func.generic
  (:gen-class)
  (:require [com.shudi.func.consts :as consts]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [clojure.java.jdbc :as jdbc]
            [clojure.data.json :as json]
            [clojure.walk :as walk]
            [clojure.edn :as edn]
            [clj-time.local :as local]
            [clj-time.coerce :as coerce])
  (:import (java.sql Connection PreparedStatement)
           (com.shudi.func.utils SigUtil DateUtil Arithmetic)
           (java.text SimpleDateFormat DecimalFormat)
           (java.util Date Base64 Map)
           (java.util.regex Pattern)
           (java.nio.charset StandardCharsets)
           (java.net URLEncoder)
           (java.math RoundingMode)))

(defn plus
  "   函数：plus(a,b)
   说明：两个数值相加,返回相加结果的数值
   参数：a/b均为必填,分别表示两个需相加的值,a+b
   作用域：字段
   分类：运算
   举例：plus(2,2) => 4;plus(3,2) => 5 "
  {:fn-type    (:compute consts/fn-type)
   :fn-chinese "相加"
   :scope      (:column consts/scope)}
  [^{:desc "数字 a"} a ^{:desc "数字 b"} b]
  (clojure.core/+ (comm/try-parse-num a)
                  (comm/try-parse-num b)))

(defn subtract
  "   函数：subtract(a,b)
   说明：两个数值相减,返回相减结果的数值
   参数：a/b均为必填,分别表示两个需相减的值,a-b
   作用域：字段
   分类：运算
   举例：subtract(2,2) => 0;subtract(3,9) => -6 "
  {:fn-type    (:compute consts/fn-type)
   :fn-chinese "相减"
   :scope      (:column consts/scope)}
  [^{:desc "数字 a"} a ^{:desc "数字 b"} b]
  (clojure.core/- (comm/try-parse-num a) (comm/try-parse-num b)))


(defn multiply
  "   函数：multiply(a,b)
   说明：两个数值相乘,返回相乘结果的数值
   参数：a/b均为必填,分别表示两个需相乘的值,a*b
   作用域：字段
   分类：运算
   举例：multiply(2,2) => 4;multiply(3,9) => 27"
  {:fn-type    (:compute consts/fn-type)
   :fn-chinese "相乘"
   :scope      (:column consts/scope)}
  [^{:desc "数字 a"} a ^{:desc "数字 b"} b]
  (clojure.core/* (comm/try-parse-num a) (comm/try-parse-num b)))

(defn divide
  "   函数：divide(a,b)
   说明：两个数值相除,返回相乘结果的数值
   参数：a/b均为必填,分别表示两个需相除的值,a/b,b不能为0
   作用域：字段
   分类：运算
   举例：divide(2,2) => 1;divide(9,3) => 3 "
  {
   :fn-type    (:compute consts/fn-type)
   :fn-chinese "相除"
   :scope      (:column consts/scope)}
  [^{:desc "数字 a"} a ^{:desc "数字 b"} b]
  (clojure.core// (comm/try-parse-num a) (comm/try-parse-num b)))

(defn arithmetic
  "   函数：arithmetic(1 + (2 - 3) * 5 / 6)
   说明：四则运算
   参数：more 为可变参数
   作用域：字段
   分类：运算
   举例：arithmetic(\"1\" \"+\" \"2\" \"/\" \"3\") => 5/3 "
  [& ^{:desc "四则运算元素"} more]
  (eval
    (edn/read-string
      (Arithmetic/calculate
        (str/join more)))))

(defn dict-sort
  "   函数：dict-sort(coll)
  说明：对集合coll内的元素进行字典排序
  参数：coll为一个集合: []或{}
  作用域：字段
  分类：运算
  举例：dict-sort([\"b\" \"a\" \"2\" \"1\"]) => (\"1\" \"2\" \"a\" \"b\") "
  {
   :fn-type    (:compute consts/fn-type)
   :fn-chinese "字典排序"
   :scope      (:column consts/scope)}
  [^{:desc "数组或映射"} coll]
  (sort coll))

(defn dict-sort-desc
  "   函数：dict-sort-desc(coll)
  说明：对集合coll内的元素进行字典倒序
  参数：coll为一个集合: []或{}
  作用域：字段
  分类：运算
  举例：dict-sort([\"b\" \"a\" \"2\" \"1\"]) => (\"b\" \"a\" \"2\" \"1\") "
  {
   :fn-type    (:compute consts/fn-type)
   :fn-chinese "字典倒序"
   :scope      (:column consts/scope)}
  [^{:desc "数组或映射"} coll]
  (reverse (sort coll)))

(defn join-vec
  "   函数：join-vec(separator,coll)
 说明：通过分隔符将集合内的元素连接起来
 参数：coll为一个数组
 作用域：字段
 分类：运算
 举例：join-vec(\"=\",[\"b\" \"a\" \"2\" \"1\"]) => b=a=2=1 "
  {
   :fn-type    (:compute consts/fn-type)
   :fn-chinese "连接集合"
   :scope      (:column consts/scope)}
  [^{:desc "分隔符"} separator ^{:desc "数组"} coll]
  (str/join separator coll))

(defn join-kv
  "   函数：join-kv(kv-separator,separator,coll)
 说明：通过kv分隔符将映射内的元素连接起来,然后通过separator将结果连接起来
 参数：coll为一个映射类型
 作用域：字段
 分类：运算
 举例：(join-kv \"=\",\"&\",{\"a\" 1 \"b\" 2}) => \"a=1&b=2\" "
  {
   :fn-type    (:compute consts/fn-type)
   :fn-chinese "连接映射"
   :scope      (:column consts/scope)}
  [^{:desc "映射分隔符"} kv-separator ^{:desc "数组分隔符"} separator ^{:desc "映射"} coll]
  (str/join separator
            (reduce-kv (fn [m k v]
                         (conj m (str/join kv-separator [k v])))
                       [] coll)))

(defn upper-case
  "   函数：upper-case(s)
 说明：小写字母转大写
 参数：s为字符串
 作用域：字段
 分类：运算
 举例：(upper-case \"a\") => \"A\" \""
  {
   :fn-type    (:str consts/fn-type)
   :fn-chinese "转大写"
   :scope      (:column consts/scope)}
  [^{:desc "字符串"} s]
  (str/upper-case (str s)))

(defn lower-case
  "   函数：lower-case(s)
 说明：小写字母转大写
 参数：s为字符串
 作用域：字段
 分类：运算
 举例：(lower-case \"A\") => \"a\" "
  {
   :fn-type    (:str consts/fn-type)
   :fn-chinese "转小写"
   :scope      (:column consts/scope)}
  [^{:desc "字符串"} s]
  (str/lower-case (str s)))

(defn to-json
  "   函数：to-json(m)
 说明：对象转json
 参数：m为集合
 作用域：字段
 分类：运算
 举例：(to-json {:name 1}) => \"{\"name\":1}\" "
  {
   :fn-type    (:str consts/fn-type)
   :fn-chinese "对象转json"
   :scope      (:column consts/scope)}
  [^{:desc "数组或映射"} m] (json/write-str m :escape-unicode false :escape-slash false))

(defn from-json
  "   函数：from-json(json)
 说明：json转对象
 参数：j为json字符串
 作用域：字段
 分类：运算
 举例：(from-json \"{\"name\":1}\" ) =>  {:name 1}"
  {
   :fn-type    (:str consts/fn-type)
   :fn-chinese "json转对象"
   :scope      (:column consts/scope)}
  [^{:desc "json字符串"} j] (json/read-str j))

(defn b64-decode
  "   函数：b64-decode(s)
说明：base64解码
参数：s为字符串
作用域：字段
分类：运算
举例：b64-decode(\"YWFhYWE=\") => aaaaa "
  {
   :fn-type    (:str consts/fn-type)
   :fn-chinese "base64解码"
   :scope      (:column consts/scope)}
  [^{:desc "base64字符串"} s]
  (String. (.decode (Base64/getDecoder) ^String s)
           StandardCharsets/UTF_8))

(defn b64-encode
  "   函数：b64-encode(s)
  说明：base64编码
  参数：s为字符串
  作用域：字段
  分类：运算
  举例：(b64-encode \"aaaaa\") => YWFhYWE= "
  {
   :fn-type    (:str consts/fn-type)
   :fn-chinese "base64编码"
   :scope      (:column consts/scope)}
  [^{:desc "字符串"} s]
  (String. (.encode (Base64/getEncoder)
                    (.getBytes ^String s StandardCharsets/UTF_8))))

(defn md5sum
  "   函数：md5sum(s)
说明：计算字符串的校验码
参数：s为字符串
作用域：字段
分类：运算
举例：md5sum(\"abc\") => 900150983cd24fb0d6963f7d28e17f72 "
  {
   :fn-type    (:sig consts/fn-type)
   :fn-chinese "md5校验"
   :scope      (:column consts/scope)}
  [^{:desc "字符串"} s]
  (SigUtil/md5sum
    (str s)))

(defn sha1sum
  "   函数：sha1sum(s)
说明：计算字符串的校验码
参数：s为字符串
作用域：字段
分类：运算
举例：sha1sum(\"abc\") => a9993e364706816aba3e25717850c26c9cd0d89d "
  {
   :fn-type    (:sig consts/fn-type)
   :fn-chinese "sha1校验"
   :scope      (:column consts/scope)}
  [^{:desc "字符串"} s]
  (SigUtil/sha1sum (str s)))

(defn sha256sum
  "   函数：sha256sum(s)
 说明：计算字符串的校验码
 参数：s为字符串
 作用域：字段
 分类：运算
 举例：sha256sum(\"abc\") => ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad "
  {
   :fn-type    (:sig consts/fn-type)
   :fn-chinese "sha256校验"
   :scope      (:column consts/scope)}
  [^{:desc "字符串"} s]
  (SigUtil/sha256sum (str s)))

(defn num-max
  "   函数：num-max(a,b)
   说明：对比两个数值的的大小,返回两个数字的最大值
   参数：a/b均为必填,分别表示两个需对比的数值
   作用域：字段
   分类：运算
   举例：num-max(10,1) => 10"
  {
   :fn-type    (:compute consts/fn-type)
   :fn-chinese "求最大值"
   :scope      (:column consts/scope)}
  [^{:desc "数字"} a ^{:desc "数字"} b]
  (clojure.core/max (comm/try-parse-num a) (comm/try-parse-num b)))

(defn num-min
  "   函数：num-min(a,b)
   说明：对比两个数值的的大小,返回两个数字的最小值
   参数：a/b均为必填,分别表示两个需对比的数值
   作用域：字段
   分类：运算
   举例：num-min(10,1) => 1"
  {
   :fn-type    (:compute consts/fn-type)
   :fn-chinese "求最小值"
   :scope      (:column consts/scope)}
  [^{:desc "数字"} a ^{:desc "数字"} b]
  (clojure.core/min (comm/try-parse-num a) (comm/try-parse-num b)))

(defmacro or*
  "   函数：or* (test1,test2)
   说明：逻辑表达式or
   参数：test1,test2只要成立一个条件则为:真
   作用域：字段
   分类：运算
   举例：or*(eq(1 1),eq(2 1)) => true"
  {
   :fn-type    (:compute consts/fn-type)
   :fn-chinese "或"
   :scope      (:column consts/scope)}
  [^{:desc "逻辑表达式1"} test1 ^{:desc "逻辑表达式2"} test2]
  (or test1 test2))

(defmacro and*
  "   函数：and* (test1,test2)
   说明：逻辑表达式and
   参数：test1,test2只有两个表达式都为真,才为真
   作用域：字段
   分类：运算
   举例：and*(eq(1 1),eq(2 2)) => true"
  {
   :fn-type    (:compute consts/fn-type)
   :fn-chinese "与"
   :scope      (:column consts/scope)}
  [^{:desc "逻辑表达式1"} test1 ^{:desc "逻辑表达式2"} test2]
  (and test1 test2))

(defn not*
  "   函数：not* (test1)
   说明：逻辑表达式not
   参数：test1 为真,则假,为假,则真
   作用域：字段
   分类：运算
   举例：not*(eq(1 2)) => true"
  {
   :fn-type    (:compute consts/fn-type)
   :fn-chinese "非"
   :scope      (:column consts/scope)}
  [^{:desc "逻辑表达式"} test1]
  (not test1))

(defn eq
  "   函数：eq(a,b)
   说明：判断左操作值是否等于右操作值,返回boolean值
   参数：a/b均为必填,分别表示两个需对比的值
   作用域：字段
   分类：运算
   举例：eq(2,2) => true;eq(3,2) => false "
  {
   :fn-type    (:condition consts/fn-type)
   :fn-chinese "是否相等"
   :scope      (:column consts/scope)}
  [^{:desc "对象"} a ^{:desc "对象"} b]
  (clojure.core/= a b))

(defn not-eq
  "   函数：not-eq(a,b)
   说明：判断左操作值是否不等于右操作值,返回boolean值
   参数：a/b均为必填,分别表示两个需对比的值
   作用域：字段
   分类：运算
   举例：not-eq(2,2) => false;not-eq(3,2) => true"
  {
   :fn-type    (:condition consts/fn-type)
   :fn-chinese "是否不相等"
   :scope      (:column consts/scope)}
  [^{:desc "对象"} a ^{:desc "对象"} b]
  (clojure.core/not= a b))

(defn lt
  "   函数：lt(a,b)
   说明：判断左操作数的值是否小于右操作数的值,返回boolean值
   参数：a/b均为必填,分别表示两个需对比的数值
   作用域：字段
   分类：运算
   举例：lt(1,2) => true "
  {
   :fn-type    (:condition consts/fn-type)
   :fn-chinese "小于"
   :scope      (:column consts/scope)}
  [^{:desc "对象"} a ^{:desc "对象"} b]
  (clojure.core/< (comm/try-parse-num a) (comm/try-parse-num b)))

(defn le
  "   函数：le(a,b)
   说明：判断左操作数的值是否小于或等于右操作数的值,返回boolean值
   参数：a/b均为必填,分别表示两个需对比的数值
   作用域：字段
   分类：运算
   举例：le(2,2) => true;le(3,2) => false  "
  {
   :fn-type    (:condition consts/fn-type)
   :fn-chinese "小于等于"
   :scope      (:column consts/scope)}
  [^{:desc "对象"} a ^{:desc "对象"} b]
  (clojure.core/<= (comm/try-parse-num a) (comm/try-parse-num b)))

(defn gt
  "   函数：gt(a,b)
   说明：判断左操作数的值是否大于右操作数的值,返回boolean值
   参数：a/b均为必填,分别表示两个需对比的数值
   作用域：字段
   分类：运算
   举例：gt(3,2) => true "
  {
   :fn-type    (:condition consts/fn-type)
   :fn-chinese "大于"
   :scope      (:column consts/scope)}
  [^{:desc "对象"} a ^{:desc "对象"} b]
  (clojure.core/> (comm/try-parse-num a) (comm/try-parse-num b)))

(defn ge
  "   函数：ge(a,b)
   说明：判断左操作数的值是否大于或等于右操作数的值,返回boolean值
   参数：a/b均为必填,分别表示两个需对比的数值
   作用域：字段
   分类：运算
   举例：ge(2,2) => true；ge(3,2) => true  "
  {
   :fn-type    (:condition consts/fn-type)
   :fn-chinese "大于等于"
   :scope      (:column consts/scope)}
  [^{:desc "对象"} a ^{:desc "对象"} b]
  (clojure.core/>= (comm/try-parse-num a) (comm/try-parse-num b)))

(defmacro if-then
  "   函数：if-then(test,body)
     说明：条件判断语句,如果test表达式为真,则执行body内容的表达式
     参数：test/boby为必填,test表示条件表达式,body表示条件表达式为真时执行内容
     作用域：字段
     分类：逻辑
     举例：if-then(eq(1 1),\"a\")=> \"a\"；"
  {:fn-type    (:process consts/fn-type)
   :fn-chinese "条件(if-then)"
   :scope      (:column consts/scope)}
  [^{:desc "逻辑表达式"} test ^{:desc "表达式"} body]
  (list 'if test body))

(defmacro if-else
  "   函数：if-else(test,body,else)
   说明：条件判断语句,如果test表达式为真,则执行body内容的表达式,否则执行else内容表达式
   参数：test/boby/else为必填,test表示条件表达式,body表示条件表达式为真时执行内容,else表示条件表达式为假时执行内容
   作用域：字段
   分类：逻辑
   举例：if-else(eq(2 1),\"a\",\"b\")=> \"b\"；"
  {:fn-type    (:process consts/fn-type)
   :fn-chinese "条件判断(if-else)"
   :scope      (:column consts/scope)}
  [^{:desc "逻辑表达式"} test ^{:desc "表达式"} body ^{:desc "表达式"} else]
  `(if ~test ~body ~else))

(defn str-join
  "   函数：str-join(s1,s2,s3)
   说明：连接参数中的字符串,返回连接之后新字符串
   参数：s1/s2/s3为必填,需要连接的字符串
   作用域：字段
   分类：运算
   举例：str-join(\"hello \",\"shenzhen \",\"!\") => \"hello shenzhen !\""
  {:fn-type    (:str consts/fn-type)
   :fn-chinese "字符串连接"
   :scope      (:column consts/scope)}
  [^{:desc "字符串"} s1 ^{:desc "字符串"} s2 ^{:desc "字符串"} s3]
  (str s1 s2 s3))

(defn str-split
  "   函数：str-split(s,re)
   说明：根据分割符,分割字符串,特殊字符'|'需要双转义'\\|'
   参数：s/re为必填,s为字符串,re为表达式
   作用域：字段
   分类：运算
   举例：str-split(\"20180012|2019001|2019002\",\"\\|\" ) => [\"20180012\" \"2019001\" \"2019002\"] "
  {:fn-type    (:str consts/fn-type)
   :fn-chinese "字符串连接"
   :scope      (:column consts/scope)}
  [^{:desc "字符串"} s ^{:desc "分割符"} re]
  (str/split ^String s (Pattern/compile re)))

(defn merge*
  "   函数：merge*(s-map,t-map)
   说明：将源的的数据覆盖到目标,如果目标map key不存在就增加,存在则保持不变。
   参数：s-map 源map,t-map 目标map
   作用域：字段
   分类：运算
   举例：merge*({:a 2 :b 2},{:a 1 :b 2 :c 3})=>{:a 2 :b 2 :c 3}"
  {:fn-type    (:process consts/fn-type)
   :fn-chinese "合并"
   :scope      (:column consts/scope)}
  [^{:desc "源map"} s-map ^{:desc "目标map"} t-map]
  (clojure.core/merge (or t-map {}) (or s-map {})))

(defn get-in*
  "   函数：get-in*(json,path)
  说明：根据path访问json对象的值
  参数：json字符串, path为路径,如data.name
  作用域：字段
  分类：运算
  举例：(get-in* \"{\\n  \\\"path\\\": {\\n    \\\"pre\\\": \\\"/v1/data/objects\\\",\\n    \\\"apiName\\\": \\\"/customer\\\",\\n    \\\"suf\\\": \\\"/description\\\"\\n  }\\n}\",\"path.pre\") ==> \"/v1/data/objects\""
  {:fn-type    (:str consts/fn-type)
   :fn-chinese "json访问"
   :scope      (:column consts/scope)}
  [^{:desc "json"} json ^{:desc "访问路径"} path]
  (get-in (if (string? json) (json/read-str json) json)
          (walk/postwalk #(comm/truly-val %)
                         (str/split ^String path #"\."))))

(defn zipmap*
  "   函数：zipmap*(keys,vals)
  说明：将keys中的每一个元素与vals中的对应元素
  参数：keys数组,vals数组
  作用域：字段
  分类：运算
  举例：zipmap*([\"name\" \"age\"],[\"sean\" \"123\"])=> {\"name\" \"sean\", \"age\" \"123\"}"
  {:fn-type    (:str consts/fn-type)
   :fn-chinese "拉链映射"
   :scope      (:column consts/scope)}
  [^{:desc "key数组"} keys ^{:desc "值数组"} vals]
  (loop [map {}
         ks (seq keys)
         vs (seq vals)]
    (if (and ks vs)
      (recur (let [k (first ks)
                   m (if (string? k)
                       (assoc-in map (str/split ^String k #"\.") (first vs))
                       (assoc map k (first vs)))]
               m)
             (next ks)
             (next vs))
      map)))


(defn group-by*
  "   函数：group-by*(keys,result-key,coll)
 说明：将一个数组内的对象，根据其主键进行分组,并将结果写入到result-key中
 参数：keys数组,result-key字符,coll数组
 作用域：字段
 分类：运算
 举例：(group-by* [\"a\" \"data\"],\"cc\",[{\"a\" 1 \"data\" \"c\"} {\"a\" 2 \"data\" \"b\"}{\"a\" 1 \"data\" \"b\"}]) =>\n[{\"a\" \"1\", \"data\" \"c\", \"cc\" [{\"a\" 1, \"data\" \"c\"}]}\n {\"a\" \"2\", \"data\" \"b\", \"cc\" [{\"a\" 2, \"data\" \"b\"}]}\n {\"a\" \"1\", \"data\" \"b\", \"cc\" [{\"a\" 1, \"data\" \"b\"}]}]"
  {:fn-type    (:str consts/fn-type)
   :fn-chinese "分组函数"
   :scope      (:column consts/scope)}
  [^{:desc "需要分组的key列表"} keys ^{:desc "分组后结果对应的key"} result-key ^{:desc "对象集合"} coll]
  (reduce-kv
    (fn [m k v]
      (conj m (-> keys
                  (zipmap (str/split k #":"))
                  (assoc result-key v)))) []
    (group-by (fn [x] (str/join ":" (map #(get-in x (str/split ^String % #"\.")) keys))) coll)))

(defn is-empty?
  "   函数：is-empty?(x)
   说明：判断对象是否为空
   参数：x为必填,任意类型
   作用域：字段
   分类：运算
   举例：is-empty?([]) => true"
  {:fn-type    (:cast consts/fn-type)
   :fn-chinese "是否为空"
   :scope      (:column consts/scope)}
  [x]
  (empty? x))

(defn to-str
  "   函数：to-str(s)
   说明：将其他格式的内容转换为字符串,返回转换后的字符串
   参数：s为必填,需转换的其他格式内容
   作用域：字段
   分类：运算
   举例：to-str(12) => 12"
  {:fn-type    (:cast consts/fn-type)
   :fn-chinese "字符串转换"
   :scope      (:column consts/scope)}
  [^{:desc "对象"} s]
  (str s))

(defn to-int
  "   函数：to-int(s)
   说明：将其他格式的内容转换为整型,返回转换后的整型int数值
   参数：s为必填,需转换的其他格式内容
   作用域：字段
   分类：运算
   举例：to-int(12) => 12"
  {:fn-type    (:cast consts/fn-type)
   :fn-chinese "整型转换"
   :scope      (:column consts/scope)}
  [^{:desc "字符串"} s]
  (cond (string? s)
        (.intValue (Integer/parseInt s))
        (ratio? s) (int s)))

(defn to-float
  "   函数：to-float(s)
   说明：将其他格式的内容转换为浮点型,返回转换后的浮点型float数值
   参数：s为必填,需转换的其他格式内容
   作用域：字段
   分类：运算
   举例：to-float(12.88) => 12.88"
  {:fn-type    (:cast consts/fn-type)
   :fn-chinese "浮点型转换"
   :scope      (:column consts/scope)}
  [^{:desc "字符串"} s]
  (.doubleValue (Double/parseDouble s)))

(defn to-date
  "   函数：to-date(fmt,date-str)
   说明：将字符串日期转换为date
   参数：fmt为日期格式yyyy-MM-dd,date-str为日期字符串2019-01-05
   作用域：字段
   分类：运算
   举例：to-date(\"yyyy-MM-dd\",\"2019-01-05\") => date"
  {:fn-type    (:cast consts/fn-type)
   :fn-chinese "字符串转日期"
   :scope      (:column consts/scope)}
  [^{:desc "日期格式"} fmt ^{:desc "日期字符串"} date-str]
  (.parse (SimpleDateFormat. fmt) date-str))

(defn date-to-str
  "   函数：date-to-str(fmt,date)
   说明：将日期转换为字符串
   参数：fmt为日期格式yyyy-MM-dd,date为日期
   作用域：字段
   分类：运算
   举例：date-to-str(\"yyyy-MM-dd\",to-date(\"yyyy-MM-dd\",\"2019-01-05\")) => 2019-01-05 "
  {:fn-type    (:cast consts/fn-type)
   :fn-chinese "日期转字符串"
   :scope      (:column consts/scope)}
  [^{:desc "日期格式"} fmt ^{:desc "日期对象"} date]
  (.format (SimpleDateFormat. fmt) date))

(defn date-to-ms
  "   函数：date-to-ms(date)
   说明：日期转毫秒
   参数：无
   作用域：字段
   分类：运算
   举例：date-to-ms(date) => 1561452359129"
  {:fn-type    (:datetime consts/fn-type)
   :fn-chinese "日期转毫秒"
   :scope      (:column consts/scope)}
  [^{:desc "日期"} d]
  (.getTime ^Date d))

(defn now-ms
  "   函数：now-ms()
   说明：获取当前的毫秒数
   参数：无
   作用域：字段
   分类：运算
   举例：now-ms() => 1561452359129"
  {:fn-type    (:datetime consts/fn-type)
   :fn-chinese "当前时间戳"
   :scope      (:column consts/scope)}
  []
  (System/currentTimeMillis))

(defn date-from-ms
  "   函数：date-from-ms(ts)
   说明：将时间戳(毫秒)转换为日期
   参数：ts 时间戳(毫秒)
   作用域：字段
   分类：运算
   举例：date-from-ms(1561452359129) => Tue Jun 25 16:46:31 CST 2019 "
  {:fn-type    (:datetime consts/fn-type)
   :fn-chinese "时间戳转日期"
   :scope      (:column consts/scope)}
  [^{:desc "时间戳(毫秒)"} ts]
  {:pre [(or (string? ts) (number? ts))]}
  (let [^Long t (cond (string? ts) (Long/parseLong ts)
                      (number? ts) ts)]
    (Date. t)))

(defn now
  "   函数：now()
    说明：获取当前的时间
    参数：无
    作用域：字段
    分类：运算
    举例：now() => Tue Jun 25 16:46:31 CST 2019 "
  {:fn-type    (:datetime consts/fn-type)
   :fn-chinese "当前时间"
   :scope      (:column consts/scope)}
  []
  (Date.))

(defn year
  "   函数：year(date)
    说明：获取给定时间的年
    参数：日期
    作用域：字段
    分类：运算
    举例：year(date) => 2019 "
  {:fn-type    (:datetime consts/fn-type)
   :fn-chinese "年"
   :scope      (:column consts/scope)}
  [^{:desc "日期对象"} date]
  (DateUtil/year date))

(defn month
  "   函数：month(date)
    说明：获取给定时间的月份
    参数：日期
    作用域：字段
    分类：运算
    举例：year(date) => 7 "
  {:fn-type    (:datetime consts/fn-type)
   :fn-chinese "月"
   :scope      (:column consts/scope)}
  [^{:desc "日期对象"} date]
  (DateUtil/month date))

(defn day-of-year
  "   函数：day-of-year(date)
    说明：获取给定时间为全年的第几天
    参数：日期
    作用域：字段
    分类：运算
    举例：day-of-year(date) => 107 "
  {:fn-type    (:datetime consts/fn-type)
   :fn-chinese "一年中第几天"
   :scope      (:column consts/scope)}
  [^{:desc "日期对象"} date]
  (DateUtil/dayOfYear date))

(defn day-of-month
  "   函数：day-of-month(date)
    说明：获取给定时间为当月的第几天
    参数：日期
    作用域：字段
    分类：运算
    举例：day-of-month(date) => 10 "
  {:fn-type    (:datetime consts/fn-type)
   :fn-chinese "一月中的第几天"
   :scope      (:column consts/scope)}
  [^{:desc "日期对象"} date]
  (DateUtil/dayOfMonth date))


(defn day-of-week
  "   函数：day-of-month(date)
    说明：获取给定时间为该周的第几天
    参数：日期
    作用域：字段
    分类：运算
    举例：day-of-week(date) => 2 "
  {:fn-type    (:datetime consts/fn-type)
   :fn-chinese "一周中的第几天"
   :scope      (:column consts/scope)}
  [^{:desc "日期对象"} date]
  (DateUtil/dayOfWeek date))

(defn hour
  "   函数：hour(date)
    说明：获取给定时间的小时
    参数：日期
    作用域：字段
    分类：运算
    举例：hour(date) => 12 "
  {:fn-type    (:datetime consts/fn-type)
   :fn-chinese "小时"
   :scope      (:column consts/scope)}
  [^{:desc "日期对象"} date]
  (DateUtil/hour date))


(defn minute
  "   函数：minute(date)
    说明：获取给定时间的小时
    参数：日期
    作用域：字段
    分类：运算
    举例：minute(date) => 12 "
  {:fn-type    (:datetime consts/fn-type)
   :fn-chinese "分钟"
   :scope      (:column consts/scope)}
  [^{:desc "日期对象"} date]
  (DateUtil/minute date))

(defn clean-map
  "   函数：clean-map(kvs)
    说明：清除空值key
    参数：映射
    作用域：字段
    分类：运算
    举例：clean-map( {\"name\" \"\"  \"cc\" \"aadd\"})\n=> {\"cc\" \"aadd\"}\n "
  {:fn-type    (:datetime consts/fn-type)
   :fn-chinese "清理映射"
   :scope      (:column consts/scope)}
  [^{:desc "映射"} kvs]
  (reduce (fn [m [k v]]
            (if (not (empty? v))
              (assoc m k v) m)) {} kvs))

(defn seconds
  "   函数：seconds(date)
    说明：获取给定时间的秒
    参数：日期
    作用域：字段
    分类：运算
    举例：seconds(date) => 12 "
  {:fn-type    (:datetime consts/fn-type)
   :fn-chinese "秒数"
   :scope      (:column consts/scope)}
  [^{:desc "日期对象"} date]
  (DateUtil/seconds date))


(defn str-replaceall
  "   函数：str-replaceall(s,regex,replacement)
   说明：替换字符串中的某个字符为新字符或字符串,返回替换之后的字符串
   参数：s为必填,原字符串;regex为必填,新字符或字符串;replacement为必填,即将被替换的字符或字符串
   作用域：字段
   分类：运算
   举例：str-replaceall(\"hello hello world\",\"hi\",\"hello\") => \"hi hi world\""
  {:fn-type    (:str consts/fn-type)
   :fn-chinese "替换所有字符串"
   :scope      (:column consts/scope)}
  [^{:desc "字符串"} s ^{:desc "正则表达式"} regex ^{:desc "字符串"} replacement]
  (.replaceAll s regex replacement))

(defn str-replace
  "   函数：str-replace(s,regex,replacement)
   说明：替换字符串中的第一个字符为新字符或字符串,返回替换之后的字符串
   参数：s为必填,原字符串;regex为必填,新字符或字符串;replacement为必填,即将被替换的字符或字符串
   作用域：字段
   分类：运算
   举例：str-replace(\"hello hello world\",\"hi\",\"hello\") => \"hi hello world\""
  {:fn-type    (:str consts/fn-type)
   :fn-chinese "替换字符串"
   :scope      (:column consts/scope)}
  [^{:desc "字符串" :tag String} s ^{:desc "正则表达式" :tag CharSequence} regex ^{:desc "字符串" :tag CharSequence} replacement]
  (.replace s regex replacement))


(defn idx-of
  "   函数：idx-of(s1,s2)
   说明：返回字符串s1中子串s2的索引,不包含则返回-1
   参数：s1/s2为必填
   作用域：字段
   分类：运算
   举例：idx-of(\"hello\",\"h\") => 0，idx-of(\"hello\",\"q\") => -1"
  {:fn-type    (:str consts/fn-type)
   :fn-chinese "返回子串索引"
   :scope      (:column consts/scope)}
  [^{:desc "字符串"} s1 ^{:desc "字符串"} s2]
  (let [i (str/index-of s1 s2)]
    (if (nil? i) -1 i)))

(defn new-vec
  "   函数：new-vec(v)
  说明：创建数组(向量),返回数组
  参数：a为必填
  作用域：字段
  分类：运算
  举例：new-vec(1) => [1]"
  {:fn-type    (:association consts/fn-type)
   :fn-chinese "新建数组"
   :scope      (:column consts/scope)}
  [^{:desc "对象"} a]
  [a])

(defn into-vec
  "   函数：into-vec(vec,item)
  说明：向一个vec插入值,返回数组
  参数：v/i为必填,v 为(new-vec 2) 创建出来的vec
  作用域：字段
  分类：运算
  举例：into-vec(new-vec(1),2) => [1 2]"
  {:fn-type    (:association consts/fn-type)
   :fn-chinese "新增数组元素"
   :scope      (:column consts/scope)}
  [^{:desc "数组"} v ^{:desc "对象"} i]
  (conj v i))


(defn into-vec2
  "   函数：into-vec2(vec,item1,item2)
  说明：向一个vec插入值,返回数组
  参数：v/i为必填,v 为(new-vec 2) 创建出来的vec
  作用域：字段
  分类：运算
  举例：into-vec2(new-vec(1),2,3) => [1 2 3]"
  {:fn-type    (:association consts/fn-type)
   :fn-chinese "新增2个数组元素"
   :scope      (:column consts/scope)}
  [^{:desc "字符串"} v ^{:desc "对象1"} i ^{:desc "对象2"} j]
  (conj v i j))

(defn new-map
  "   函数：new-map(k,v)
   说明：创建k-v键值对,返回map值
   参数：k/v为必填
   作用域：字段
   分类：运算
   举例：new-map(order_id,001) => {order_id 001}"
  {:fn-type    (:association consts/fn-type)
   :fn-chinese "新建键值对"
   :scope      (:column consts/scope)}
  [^{:desc "键"} key ^{:desc "值"} val]
  {key, val})

(defn into-map
  "   函数：into-map(map,k,v)
   说明：向一个map插入k-v值,如果存在则替换,不存在会更新,返回map组
   参数：k/v为必填,m 为(new-map a 2) 创建出来的map
   作用域：字段
   分类：运算
   举例：into-map(new-map(order_id 001),order_id,002) => {order_id 001 order_id 002}"
  {:fn-type    (:association consts/fn-type)
   :fn-chinese "插入新健值对"
   :scope      (:column consts/scope)}
  [^{:desc "映射"} m ^{:desc "键"} k ^{:desc "值"} v]
  (clojure.core/assoc m k v))

(defn into-map2
  "   函数：into-map(map,k1,v1,k2,v2)
   说明：向一个map插入k-v值,如果存在则替换,不存在会更新,返回map组
   参数：k/v为必填,m 为(new-map a 2) 创建出来的map
   作用域：字段
   分类：运算
   举例：into-map(new-map(order_id 001),order_id,002,user_id,001) => {order_id 001 order_id 002 user_id 001}"
  {:fn-type    (:association consts/fn-type)
   :fn-chinese "插入2条映射"
   :scope      (:column consts/scope)}
  [^{:desc "映射"} m ^{:desc "键1"} k1 ^{:desc "值1"} v1 ^{:desc "键2"} k2 ^{:desc "值2"} v2]
  (clojure.core/assoc m k1 v1 k2 v2))

(defn ele->map
  "   函数：ele->map(m,ks,coll)
   说明：将数组里面的元素转换为映射
   参数：m为(new-map a 2)创建出来的map,ks为数组,m里面里面的keys,coll为元素数组
   作用域：记录级
   分类：运算
   举例：ele->map({\"a\" \"b\" \"c\" {\"d\" \"\"}},[\"c\" \"d\"],[1 2 3]) => [{\"a\" \"b\" \"c\" {\"d\" 1}} {\"a\" \"b\" \"c\" {\"d\" 2}} {\"a\" \"b\" \"c\" {\"d\" 3}}]"
  {:fn-type    (:association consts/fn-type)
   :fn-chinese "数组元素转映射"
   :scope      (:record consts/scope)}
  [^{:desc "映射"} m ^{:desc "映射路径"} ks ^{:desc "集合"} coll]
  {:pre [(associative? m) (vector? ks) (coll? coll)]}
  (mapv #(assoc-in m ks %) coll))

(defmacro foreach
  "   函数：foreach(x,body,coll)
   说明：遍历集合内的每个元素，传入body内执行,并返回一个集合
   参数：x为一个变量,body为执行的函数块,coll为元素数组
   作用域：记录级
   分类：运算
   举例：foreach(x,plus(x 1),[1 2 3]) => [2 3 4]"
  {:fn-type    (:association consts/fn-type)
   :fn-chinese "遍历元素"
   :scope      (:record consts/scope)}
  [^{:desc "变量"} x ^{:desc "函数块"} body ^{:desc "集合"} coll]
  {:pre [(string? x)]}
  `(vec (for [~(symbol x) ~coll]
          ~(walk/postwalk #(if (= % (name (symbol x)))
                             (symbol %) %) body))))

(defn sql-query
  "   函数：sql-query(conn,table,column,where)
   说明：SQL查询函数,根据条件查询指定数据库与数据表的字段,返回值为字段的值
   参数：conn/table/column为必填;conn为节点数据库,默认为数据中心的中间库;table为表名,可以是一个对象模型;column为返回字段;where为查询条件,格式为健值对,如{key1 vlaue1 key2 value2} =>key1=value1 and key2=value2
   作用域：字段
   分类：关联
   举例：sql-query(data_conn,order,order_total_price,{order_id 1}) => select order_total_price from order where order_id=1;sql-query(data_conn,order,\"sum(order_price) as total\",{order_id 1}) => select sum(order_price) as total from order where order_id=1"
  {:fn-type    (:association consts/fn-type)
   :fn-chinese "SQL查询"
   :scope      (:column consts/scope)}
  [^{:desc "数据库连接"} conn ^{:desc "表"} table ^{:desc "字段"} column ^{:desc "条件"} where]
  {:pre [(not (nil? conn))
         (or (symbol? column) (and (string? column) (not (str/index-of column "*"))))
         (and (map? where) (not-empty where))]}
  (letfn [(set-stmt-params
            [stmt params]
            {:pre [(some? stmt)]}
            (loop [idx 1 p params]
              (if (seq p)
                (do (jdbc/set-parameter (first p) stmt idx) (recur (inc idx) (next p)))
                stmt)))]
   (let [col (str "`" (str/trim column) "`")
         params (mapv (fn [[_ v]] v) where)
         cd (str/join " and " (mapv (fn [[k _]] (str/join "=" [(str "`" (str/trim k) "`") "?"])) where))
         sql (str/join " " ["select" col "from" (str "`" (str/trim table) "`") "where" cd])]
     (log/debug "query-sql " sql params)
     (let [stmt ^PreparedStatement (jdbc/prepare-statement ^Connection conn sql)]
       (let [rs (.executeQuery (set-stmt-params stmt params))
             r (jdbc/result-set-seq rs {:keywordize? false})]
         (when (seq r)
           (get (first r) (first (keys (first r))))))))))


(defn sql-update
  "   函数：sql-update(conn,table,column,where)
   说明：SQL更新语句,根据条件更新某字段至数据库中的数据表
   参数：conn/table/column为必填;conn为节点数据库,默认为数据中心的中间库;table为表名,可以是一个对象模型;column为返回字段;where为查询条件,格式为健值对,如{key1 vlaue1 key2 value2} =>key1=value1 and key2=value2
   作用域：字段
   分类：关联
   举例：sql-update(data_conn,order,order_total_price,{order_id 1}) => update order set order_total_price where order_id=1"
  {:fn-type    (:association consts/fn-type)
   :fn-chinese "SQL更新"
   :scope      (:column consts/scope)
   }
  [^{:desc "数据库连接"} conn ^{:desc "表"} table ^{:desc "字段"} column ^{:desc "条件"} where]
  {:pre [(not (nil? conn))
         (and (map? column) (not-empty column))
         (and (map? where) (not-empty where))]}
  (letfn [(set-stmt-params
            [stmt params]
            {:pre [(some? stmt)]}
            (loop [idx 1 p params]
              (if (seq p)
                (do (jdbc/set-parameter (first p) stmt idx) (recur (inc idx) (next p)))
                stmt)))]
   (let [col (str/join " , " (mapv (fn [[k _]] (str/join "=" [(str "`" (str/trim k) "`") "?"])) column))
         u-params (mapv (fn [[_ v]] (str v)) column)
         cd (str/join " and " (mapv (fn [[k _]] (str/join "=" [(str "`" (str/trim k) "`") "?"])) where))
         w-params (mapv (fn [[_ v]] (str v)) where)
         sql (str/join " " ["update" (str "`" (str/trim table) "`") "set" col "where" cd])
         params (into [] (concat u-params w-params))]
     (log/debug "update-sql-fun " sql u-params w-params)
     (let [stmt ^PreparedStatement (jdbc/prepare-statement ^Connection conn sql)]
       (let [rs (.executeUpdate (set-stmt-params stmt params))]
         rs)))))

(defn len
  "   函数：len(coll)
  说明：返回字符个数
  参数：coll为必填，需转换的内容
  作用域：字段
  分类：运算
  举例：len(\"123456\") => 6;len(123) => 3;len([1 2 3] => 3)"
  {:fn-type    (:cast consts/fn-type)
   :fn-chinese "长度"
   :scope      (:column consts/scope)}
  [^{:desc "对象"} coll]
  (if (number? coll)
    (count (str coll))
    (count coll)))

(defn index-of-coll
  "   函数：index-of-coll(coll,ele)
  说明：从coll对象中返回ele子对象第一次出现的位置,不包含则返回-1
  参数：coll/ele为必填,coll为源对象,ele为子对象
  作用域：字段
  分类：运算
  举例：instr(\"123456\",\"1\") => 1;instr([\"1\" \"2\" \"3\"] \"2\") => 2"
  {:fn-type    (:process consts/fn-type)
   :fn-chinese "位置"
   :scope      (:column consts/scope)}
  [^{:desc "对象"} coll ^{:desc "子对象"} ele]
  {:pre [(not (empty? coll))
         (and (not (empty? ele)))
         ]}
  (if (= (.indexOf coll ele) -1) -1 (inc (.indexOf coll ele))))

(defn left
  "   函数：left(coll,len)
  说明：返回coll左边len长度的子对象
  参数：coll/len为必填,coll为对象,len为长度
  作用域：字段
  分类：运算
  举例：left(\"123456\",1) => \"1\";left([1 2 3],1)=>[1]"
  {:fn-type    (:process consts/fn-type)
   :fn-chinese "左子对象"
   :scope      (:column consts/scope)}
  [^{:desc "对象"} coll ^{:desc "长度"} len]
  {:pre [(not (empty? coll))
         (and (number? len))
         ]}
  (let [l (count coll)] (cond
                          (string? coll) (if (>= l len) (.substring coll 0 len) coll)
                          (<= l len) coll
                          (> l len)
                          (loop [idx 0 rs (empty coll) c (seq coll)]
                            (if (and c (> len idx))
                              (recur (inc idx) (conj rs (first c)) (next c))
                              rs))))
  )

(defn right
  "   函数：right(coll,len)
  说明：返回coll右边len长度的子对象
  参数：coll/len为必填,coll为对象,len为长度
  作用域：字段
  分类：运算
  举例：right(\"123456\",1) => \"6\";right([1 2 3],1)=>[3]"
  {:fn-type    (:process consts/fn-type)
   :fn-chinese "右子对象"
   :scope      (:column consts/scope)}
  [^{:desc "对象"} coll ^{:desc "长度"} len]
  {:pre [(not (empty? coll))
         (and (number? len))
         ]}
  (let [l (count coll)] (cond
                          (string? coll) (if (>= l len) (.substring coll (- l len)) coll)
                          (<= l len) coll
                          (> l len)
                          (loop [idx 0 rs (empty coll) c (reverse (seq coll))]
                            (if (and c (> len idx))
                              (recur (inc idx) (conj rs (nth c (dec (- len idx)))) c)
                              rs))))
  )
(defn sub-coll
  "   函数：sub-coll(coll,pos,len)
  说明：返回coll对象从pos开始len长度的子对象,支持字符串和集合
  参数：coll/pos/len为必填,coll为对象,pos为开始位置,len为长度
  作用域：字段
  分类：运算
  举例：substring(\"123456\",2,2) => \"23\";substring([1 2 3],2,2)=>[2 3]"
  {:fn-type    (:process consts/fn-type)
   :fn-chinese "子对象"
   :scope      (:column consts/scope)}
  [^{:desc "对象"} coll ^{:desc "位置"} pos ^{:desc "长度"} len]
  {:pre [(not (empty? coll))
         (and (number? pos))
         (and (number? len))
         ]}
  (let [l (count coll)
        sub (fn [] (.substring coll (- pos 1)))]
    (cond
      (string? coll)
      (cond (< l pos) ""
            (<= l len) (sub)
            (= l pos) (sub)
            :else (.substring coll (- pos 1) (inc len)))
      (coll? coll)
      (cond
        (< l pos) (empty coll)
        (>= l pos)
        (loop [idx 0 rs (empty coll) c (seq coll)]
          (if (and c (> len idx) (and (<= idx (- l pos))))
            (recur (inc idx) (->> (-> (dec pos) (+ idx)) (nth c) (conj rs)) c) rs))))))

(defn map-values
  "   函数：map-values(map)
   说明：获取map中所有键的值
   参数：map
   作用域：字段
   分类：运算
   举例：map-values(map) => [1 2 3]"
  {:fn-type    (:cast consts/fn-type)
   :fn-chinese "map中所有键的值"
   :scope      (:column consts/scope)}
  [^{:desc "对象"} map]
  (.values ^Map map))

(defn url-encoder
  "   函数：url-encoder(str)
   说明：url-encoder转换
   参数：str为字符串
   作用域：字段
   分类：运算
   举例：url-encoder(\"123\") => [1 2 3]"
  {:fn-type    (:cast consts/fn-type)
   :fn-chinese "URLEncoder"
   :scope      (:column consts/scope)}
  [^{:desc "字符串"} str]
  (URLEncoder/encode str "utf-8"))

(defn abs
  "函数：(abs s)
   说明：绝对值
   参数：s为必填,必须为数字
   作用域：字段
   分类：运算
   举例：(abs -1) => 1"
  {:fn-type    (:cast consts/fn-type)
   :fn-chinese "绝对值"
   :scope      (:column consts/scope)}
  [^{:desc "数字"} s]
  (Math/abs s))

(defn decimal-format
  "函数：(decimal-format s f)
   说明：数值格式化
   参数：s为必填数字,f为格式化字符串
   作用域：字段
   分类：运算
   举例：(decimal-format 1.234 \"0.00\") => 1.23"
  {:fn-type    (:cast consts/fn-type)
   :fn-chinese "数值格式化"
   :scope      (:column consts/scope)}
  [^{:desc "数字"} s ^{:desc "字符串"} f]
  (.format (DecimalFormat. f) s))

(defn time-diff
  "函数：(time-diff u s1 s2)
   说明：计算时间差
   参数：u为必填相差时间单位，天时分秒，s1,s2为必填时间字符串
   作用域：字段
   分类：运算
   举例：(time-diff \"天\" \"2020-02-03\" \"2020-02-04\") => 1"
  {:fn-type    (:cast consts/fn-type)
   :fn-chinese "计算时间差"
   :scope      (:column consts/scope)}
  [^{:desc "字符串"} u ^{:desc "字符串"} s1 ^{:desc "字符串"} s2]
  (let [diff-seconds (/ (- (coerce/to-long (local/to-local-date-time s1))
                           (coerce/to-long (local/to-local-date-time s2))) 1000)
        dt ((keyword u) consts/date-type)
        ]
    (int (/ diff-seconds dt))
    ))
(defn time-add-sub
  "函数：(time-add-sub u d s)
   说明：日期加减
   参数：u为必填相差时间单位，天时分秒，d为加减时间,s为必填时间字符串
   作用域：字段
   分类：运算
   举例：(time-add-sub \"天\" 3 \"2020-02-04\") => \"2020-02-07\" "
  {:fn-type    (:cast consts/fn-type)
   :fn-chinese "日期加减"
   :scope      (:column consts/scope)}
  [^{:desc "字符串"} u ^{:desc "数字"} d  ^{:desc "字符串"} s]
  (let [time-seconds (/ (coerce/to-long (local/to-local-date-time s)) 1000)
        as (* ((keyword u) consts/date-type) d)
        r (+ time-seconds as)
        ]
    (.format (SimpleDateFormat. "yyyy-MM-dd HH:mm:ss") (Date. (* r 1000)))
    ))

(defn str-left-add
  "函数：(str-left-add s1 l s2)
   说明：字符串左补充
   参数：s1为必填补充字符串，l长度，s2为填充字符串
   作用域：字段
   分类：运算
   举例：(str-left-add \"123\" 5 \"0\") => 00123"
  {:fn-type    (:cast consts/fn-type)
   :fn-chinese "字符串左补充"
   :scope      (:column consts/scope)}
  [^{:desc "字符串"} s1 ^{:desc "数字"} l ^{:desc "字符串"} s2]
  (cond (> (count s1) l) (.substring s1 0 l)
        (< (count s1) l) (loop [x s1 ss s2]
                           (if (not= (count x) l)
                             (recur (str ss x) ss) x))
        :else s1))

(defn str-right-add
  "函数：(str-right-add s1 l s2)
   说明：字符串右补充
   参数：s1为必填补充字符串，l长度，s2为填充字符串
   作用域：字段
   分类：运算
   举例：(str-right-add \"123\" 5 \"0\") => 12300"
  {:fn-type    (:cast consts/fn-type)
   :fn-chinese "字符串右补充"
   :scope      (:column consts/scope)}
  [^{:desc "字符串"} s1 ^{:desc "数字"} l ^{:desc "字符串"} s2]
  (let [c (count s1)] (cond (> c l) (.substring s1 (- c l) c)
                            (< c l) (loop [x s1 ss s2]
                                      (if (not= (count x) l)
                                        (recur (str x ss) ss) x))
                            :else s1)))

(defn date-diff
  "函数：date-diff(start_date,end_date)
  名称：日期相减
  说明：计算两个日期之差，返回天。
  参数：start 支持格式 yyyy-MM-dd hh:mm:ss 与 yyyy-MM-dd,
       end   支持格式 yyyy-MM-dd hh:mm:ss 与 yyyy-MM-dd.
  作用域：字段
  分类：日期函数
  "
  {:fn-type    (:datetime consts/fn-type)
   :fn-chinese "日期相减"
   :scope      (:column consts/scope)}
  [^{:desc "开始日期"} start ^{:desc "结束日期"} end]
  (DateUtil/dateDif start end))

(defn round
  "函数: round(f, num_digits)
  名称: 返回一个 string 类型得数值，该数值是按照指定的小数位数进行四舍五入运算的结果。
  参数: f 需要进行四舍五入的字符.
        num_digits 指定的位数，按此位数进行四舍五入
          （1）如果 num_digits 大于 0，则四舍五入到指定的小数位；
          （2）如果 num_digits 等于 0，则四舍五入到最接近的整数；
          （3）如果 num_digits 小于 0，则在小数点左侧进行四舍五入。
   举例：round(3.19, 1) 将 3.19 四舍五入到一个小数位 (3.2)
        round(2.649, 1) 将 2.649 四舍五入到一个小数位 (2.6)
        round(-5.574, 2) 将 -5.574 四舍五入到两小数位 (-5.57)
        round(18.8, -1) 将 18.8 四舍五入到小数点左侧一位 (20)。这个参数-1表示取整到十位数。
         "
  {:fn-type    (:compute consts/fn-type)
   :fn-chinese "小数精度"
   :scope      (:column consts/scope)}
  [^{:desc "小数"} f ^{:desc "指定的位数"} num_digits]
  (let [a (BigDecimal. f)]
    (.doubleValue (.setScale a num_digits RoundingMode/HALF_UP))))
