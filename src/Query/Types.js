"use strict";
var __assign = (this && this.__assign) || Object.assign || function(t) {
    for (var s, i = 1, n = arguments.length; i < n; i++) {
        s = arguments[i];
        for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p))
            t[p] = s[p];
    }
    return t;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.sayHello = "hello from JS";
exports.toSql = function (f) { return f({
    country_code: ':)',
    operator_code: ''
}); };
exports.queryOptions = ({
    noTimezone: false,
    tableAlias: "us"
});
// export const select = (alias : string, timecol : string, maps: string)
exports.evalJs = function (x) { return eval(x); };
exports.myCat = { color: "gray" };
exports.mkCat = function (c, just) {
    console.log(c);
    return __assign({}, c, { color: just("red") });
};
exports.myCat1 = exports.myCat;
