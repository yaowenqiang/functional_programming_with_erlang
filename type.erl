-type expr() :: {'num', integer()}
             | {'var', atom()}
             | {'add', expr(), expr()}
             | {'mul', expr(), expr()}
