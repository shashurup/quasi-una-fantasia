# rackushka

Quasi uan fantasia is a graphical lisp shell prototype.

## Overview

> Остап поклонился, протянул вперед руки, как бы отвергая не заслуженные им аплодисменты, и взошел на эстраду.
> - Товарищи! - сказал он прекрасным голосом. - Товарищи и братья по шахматам, предметом моей сегодняшней лекции служит то, о чем я читал и, должен признаться, не без успеха в Нижнем Новгороде неделю тому назад. Предмет моей лекции - плодотворная дебютная идея. Что такое, товарищи, дебют и что такое, товарищи, идея? Дебют, товарищи, - это quasi una fantasia. А что такое, товарищи, значит идея? Идея, товарищи, - это человеческая мысль, облеченная в логическую шахматную форму. Даже с ничтожными силами можно овладеть всей доской. Все зависит от каждого индивидуума в отдельности. Например, вон тот блондинчик в третьем ряду. Положим, он играет хорошо...
 
The ideas behind the project has been stirring in the back of my head for quite a while already. To blend some things I like, such as shell, REPL and Lisp. To add some ideas on top of that, such as handling structured data in pipelines and breaking the limits of text only data representation modern shells only offer.

## Development

To get an interactive development environment run:

    lein fig:build

This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL. An easy way to try it is:

    (js/alert "Am I connected?")

and you should see an alert in the browser window.

To clean all compiled files:

	lein clean

To create a production build run:

	lein clean
	lein fig:min


## License

Copyright © 2018 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
