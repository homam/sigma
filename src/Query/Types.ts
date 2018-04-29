export const sayHello = "hello from JS"

export const toSql = f => f({
  country_code: ':)',
  operator_code: ''
})

export const queryOptions = ({
           noTimezone: false,
           tableAlias: "us"
         });

// export const select = (alias : string, timecol : string, maps: string)

export const evalJs = x => eval(x)

export const myCat = { color: "gray"}

export const mkCat = (c, just) => {
  console.log(c);
  return {...c, color: just("red")}
}

export const myCat1 = myCat