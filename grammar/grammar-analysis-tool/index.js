const firstFollow = require('first-follow')
const fs = require('fs');

const grammar = fs.readFileSync('grammar.txt').toString();

const toRule = (ruleLine) => {
   const [left, right] = ruleLine;
   return {
       left : left,
       right: right.split(' ').map(x => x === 'epsilon' ? null : x)
   }
}
const rules = grammar.split('\n')
    .filter(x => x.length)
    .map(x => 
        x.split("::=").map(y => y.trim())
    )
    .map(toRule);

const { firstSets, followSets, predictSets } = firstFollow(rules);
fs.writeFileSync('rules.txt', JSON.stringify(rules, null, 4))
fs.writeFileSync('first.txt', JSON.stringify(firstSets, null, 4))
fs.writeFileSync('follow.txt', JSON.stringify(followSets, null, 4))
fs.writeFileSync('predict.txt', JSON.stringify(predictSets, null, 4))
