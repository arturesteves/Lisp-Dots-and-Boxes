String.prototype.isEmpty = function () {
    return (this.length === 0 || !this.trim());
};
if (typeof String.prototype.contains === 'undefined') {
    String.prototype.contains = function (it) {
        return this.indexOf(it) != -1;
    };
}
String.prototype.replaceAll = function (search, replacement) {
    var target = this;
    return target.replace(new RegExp(search, 'g'), replacement);
};
Array.prototype.limpar = function () {
    for (var i = 0; i < this.length; i++) {
        if (this[i].isEmpty() || this[i] == ")") {
            this.splice(i, 1);
            i--;
        }
    }
    return this;
};
var espacamento = "   ";
var preenchido = xChars('-', espacamento.length);
$(function () {
    $("#converter").click(function () {
        var horizontal = [];
        var vertical = [];
        var atual = horizontal;
        var lista = $("#lista").val();
        if (lista[0] == "'") {
            lista = lista.substring(1, lista.length);
        }
        lista.split('(').forEach(function (valor, index) {
            if (!valor.isEmpty()) {
                valor = valor.replace(')', ' ');
                if (valor.indexOf(')') !== -1) {
                    atual.push(valor.replace(')', ' ').split(' ').limpar());
                    atual = vertical;
                } else {
                    atual.push(valor.split(' ').limpar());
                }
            }
        });
        desenharLista(horizontal, vertical);
    });
    $("#gerar").click(function () {
        var linhas = $("#linhas").val();
        var colunas = $("#colunas").val();

        if (!isNaN(linhas) && !isNaN(colunas)) {
            var output = "<div>" + espacamento + xChars(' ', espacamento.length - 1);
            for (var v = 0; v < colunas; v++) {
                output += (v + 1) + espacamento;
            }
            for (var l = 0; l <= linhas; l++) {
                output += "<div class='linha horizontal'>" + espacamento;
                for (var c = 0; c < colunas; c++) {
                    output += "<span>O</span><div class='coluna'><span class='ligacaoH " + (l + 1) + " " + (c + 1) + "'>" + espacamento + "</span></div>" + (c == colunas - 1 ? "<span>O</span>" : "");
                }
                if (l != linhas) {
                    output += "</div><div class='linha vertical '>";
                    output += (l + 1) + xChars(' ', espacamento.length - 1);
                    for (var lV = 0; lV <= linhas; lV++) {

                        output += "<div class='coluna col_" + lV + "'><span class='ligacaoV'> </span></div>" + espacamento;
                    }
                    output += "</div>";
                }
            }
            output += "</div><input type='text' id='resultado' readonly>";
            $("#construtor .corpo").html(output);
            $(".ligacaoH").click(function () {
                if ($(this).text() == espacamento) {
                    $(this).text(preenchido);
                } else {
                    $(this).text(espacamento)
                }
                criarLista();
            });
            $(".ligacaoV").click(function () {
                if ($(this).text() == " ") {
                    $(this).text("|");
                } else {
                    $(this).text(" ")
                }
                criarLista();
            });
        }
    });
});
function criarLista() {
    var arrayH = [];
    $("#construtor .corpo div.linha.horizontal").each(function (index) {
        var arrayAux = [];
        $(this).find("div.coluna").each(function (index) {
            arrayAux.push($(this).text() == preenchido ? "T" : "NIL");
        })
        arrayH.push(arrayAux);
    });
    var arrayV = [];
    $("#construtor .corpo div.linha.vertical:first .coluna").each(function (index) {
        var coluna = $(this).attr('class').replace("coluna ", "");
        var arrayAux = [];
        $("#construtor .corpo div.linha.vertical ." + coluna).each(function (index) {
            arrayAux.push($(this).text() == "|" ? "T" : "NIL");
        });
        arrayV.push(arrayAux);
    });
    var string = "'(" + arrayParaString(arrayH) + " " + arrayParaString(arrayV) + ")";


    $("#resultado").val(string);
    /*console.clear();
    console.log(arrayParaString(arrayH));
    console.log(arrayParaString(arrayV));*/
}
function arrayParaString(array) {
    var string = "(";
    array.forEach(function (item, index) {
        if (Array.isArray(item)) {
            item.forEach(function (item, index) {
                if (index == 0) {
                    string += "(";
                }
                string += item + " ";
            });
            string = string.slice(0, -1);
            string += ") ";
        } else {
            string += item + " ";
        }
    });
    string = string.slice(0, -1);
    string += ")";
    return string;
}
function desenharLista(horizontal, vertical) {

    var visualizacao = "<div class='linha'>" + xChars(' ', espacamento.length + (espacamento.length / 2));
    var textoHorizontal = "<h1>Horizontal</h1>";
    var textoVertical = "<h1>Vertical</h1>";

    var vert = $("#leitor .resultado .vertical");
    vert.html(textoVertical);
    for (var v = 0; v < horizontal[0].length; v++) {
        visualizacao += "<div class='coluna'>" + (v + 1) + xChars(' ', espacamento.length) + "</div>";
    }
    visualizacao += "</div>";
    for (var h = 0; h < horizontal.length; h++) {
        vert.append("<div class='linha' id='linha_" + h + "'>(</div>");
    }
    for (var h = 0; h < horizontal.length; h++) {
        visualizacao += "<div class='linha'>" + espacamento + "<span>";
        textoHorizontal += "<div class='linha'>(";
        textoVertical += "<div class='linha'> " + espacamento;
        for (var v = 0; v < horizontal[h].length; v++) {
            visualizacao += "O";
            if (horizontal[h][v] == "T") {
                visualizacao += "<a class='ligacaoVH Hh" + (h + 1) + " Hv" + (v + 1) + "'>" + preenchido + "</a>";
                textoHorizontal += "<a class='lista Hh" + (h + 1) + " Hv" + (v + 1) + "'>T</a>" + espacamento;
            } else {
                visualizacao += "<a class='ligacaoVH Hh" + (h + 1) + " Hv" + (v + 1) + "'>" + espacamento + "</a>";
                textoHorizontal += "<a class='lista Hh" + (h + 1) + " Hv" + (v + 1) + "'>N</a>" + espacamento;
            }
        }
        visualizacao += "O</span></div><div class='linha'><span>";
        textoHorizontal = textoHorizontal.substr(0, textoHorizontal.length - espacamento.length);
        textoHorizontal += ")</div>";
        if (h != vertical.length - 1) {
            if (h != vertical[h].length)
                visualizacao += (h + 1) + xChars(' ', espacamento.length - 1);
            for (var v = 0; v < vertical.length; v++) {
                if (vertical[v][h] == "T") {
                    visualizacao += "<a class='ligacaoVH Vh" + (h + 1) + " Vv" + (v + 1) + "'>" + "|" + "</a>" + espacamento;
                    var auxVertical = "<a class='lista Vh" + (h + 1) + " Vv" + (v + 1) + "'>T</a>" + espacamento;
                    textoVertical += auxVertical;
                    $("#linha_" + v).append(auxVertical);
                } else {
                    visualizacao += "<a class='ligacaoVH Vh" + (h + 1) + " Vv" + (v + 1) + "'> </a>" + espacamento;
                    auxVertical = "<a class='lista Vh" + (h + 1) + " Vv" + (v + 1) + "'>N</a>" + espacamento;
                    textoVertical += auxVertical;
                    $("#linha_" + v).append(auxVertical);
                }
            }
        }
        visualizacao += " </div>";
        textoVertical += "</div>" + espacamento;
    }
    $("#leitor .resultado .vertical .linha").each(function (index) {
        var conteudo = $(this).html();
        $(this).html(conteudo.substr(0, conteudo.length - 3) + ")");
    });
    $("#leitor .corpo").html(visualizacao);
    $("#leitor .resultado .horizontal").html(textoHorizontal);
    var remover = ["ligacaoVH", "lista"];
    $(".ligacaoVH, .lista").hover(function () {
        var classes = $(this).attr('class');
        var aux = (classes.contains("ligacaoVH") ? ".lista" : ".ligacaoVH");
        remover.forEach(function (item) {
            classes = classes.replace(item, "");
        });

        classes = classes.replaceAll(" ", ".");
        $(this).addClass('destacado');
        $(aux + classes).addClass('destacado');
    }, function () {
        $(".destacado").removeClass('destacado');
    });
    $(".ligacaoVH, .lista").click(function () {
        var classes = $(this).attr('class');
        var aux = (classes.contains("ligacaoVH") ? ".lista" : ".ligacaoVH");
        remover.forEach(function (item) {
            classes = classes.replace(item, "");
        });
        
        classes = classes.replaceAll(" ", ".");
        var final=aux + classes;
        if(final[final.length-1]=='.'){
            final=final.substr(0, final.length-1);
        }
        $(this).toggleClass('marcado');
        
        $(final).toggleClass('marcado');
    });

}
function xChars(char, vezes) {
    var string = "";
    for (var i = 0; i < vezes; i++) {
        string += char;
    }
    return string;
}