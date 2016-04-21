// ==UserScript==
// @name        Access Report Data
// @namespace   https://github.com/jamesjonesmath/canvancement
// @description Generates a .CSV download of the access report for all 

students
// @include     https://learn.canvas.net/courses/*/users
// @version     5
// @grant       none
// ==/UserScript==
(function () {
  'use strict';
  var userData = {
  };
  var accessData = [
  ];
  var pending = - 1;
  addAccessReportButton();
  function addAccessReportButton() {
    if ($('#jj_access_report').length == 0) {
      $('#right-side-wrapper div').append('<a id="jj_access_report" 

class="btn button-sidebar-wide"><i class="icon-analytics"></i> Access 

Report Data</a>');
      $('#jj_access_report').one('click', accessReport);
    }
    return;
  }
  function accessReport() {
    var courseId = getCourseId();
    var url = '/api/v1/courses/' + courseId + '/sections?include[]

=students&per_page=100';
    pending = 0;
    getStudents(courseId, url);
  }
  function nextURL(linkTxt) {
    var url = null;
    if (linkTxt) {
      var links = linkTxt.split(',');
      var nextRegEx = new RegExp('^<(.*)>; rel="next"$');
      for (var i = 0; i < links.length; i++) {
        var matches = nextRegEx.exec(links[i]);
        if (matches) {
          url = matches[1];
        }
      }
    }
    return url;
  }
  function getStudents(courseId, url) {
    try {
      pending++;
      $.getJSON(url, function (udata, status, jqXHR) {
        url = nextURL(jqXHR.getResponseHeader('Link'));
        for (var i = 0; i < udata.length; i++) {
          var section = udata[i];
          if (section.students.length > 0) {
            for (var j = 0; j < section.students.length; j++) {
              var user = section.students[j];
              user.section_id = section.id;
              user.section_name = section.name;
              user.sis_section_id = section.sis_section_id;
              user.sis_course_id = section.sis_course_id;
              userData[user.id] = user;
            }
          }
        }
        if (url) {
          getStudents(courseId, url);
        }
        pending--;
        if (pending <= 0) {
          getAccessReport(courseId);
        }
      }).fail(function () {
        pending--;
        throw new Error('Failed to load list of students');
        return;
      });
    } 
    catch (e) {
      errorHandler(e);
    }
  }
  function getAccessReport(courseId) {
    pending = 0;
    for (var id in userData) {
      if (userData.hasOwnProperty(id)) {
        var url = '/courses/' + courseId + '/users/' + id + 

'/usage.json?per_page=100';
        getAccesses(courseId, url);
      }
    }
  }
  function getAccesses(courseId, url) {
    try {
      pending++;
      $.getJSON(url, function (adata, status, jqXHR) {
        url = nextURL(jqXHR.getResponseHeader('Link'));
        accessData.push.apply(accessData, adata);
        if (url) {
          getAccesses(courseId, url);
        }
        pending--;
        if (pending <= 0) {
          makeReport();
        }
      }).fail(function () {
        pending--;
        console.log('Some access report data failed to load');
        if (pending <= 0) {
          makeReport();
        }
      });
    } 
    catch (e) {
      errorHandler(e);
    }
  }
  function getCourseId() {
    try {
      var courseRegex = new RegExp('/courses/([0-9]+)');
      var courseId = null;
      var matches = courseRegex.exec(window.location.href);
      if (matches) {
        courseId = matches[1];
      } 
      else {
        throw new Error('Unable to detect Course ID');
      }
    } 
    catch (e) {
      errorHandler(e);
    }
    return courseId;
  }
  function makeReport() {
    try {
      var csv = createCSV();
      if (csv) {
        var csvData = 'data:text/csv;charset=utf-8,\ufeff' + 

encodeURIComponent(csv);
        var el = document.createElement('a');
        el.setAttribute('download', 'access-report.csv');
        el.setAttribute('href', csvData);
        el.style.display = 'none';
        document.body.appendChild(el);
        el.click();
        document.body.removeChild(el);
        $('#jj_access_report').one('click', accessReport);
      } 
      else {
        throw new Error(Problemcreatingreport);
      }
    } 
    catch (e) {
      errorHandler(e);
    }
  }
  function createCSV() {
    var fields = [
      {
        'name': 'User ID',
        'src': 'u.id'
      },
      {
        'name': 'Display Name',
        'src': 'u.name'
      },
      {
        'name': 'Sortable Name',
        'src': 'u.sortable_name'
      },
      {
        'name': 'Category',
        'src': 'a.asset_category'
      },
      {
        'name': 'Class',
        'src': 'a.asset_class_name'
      },
      {
        'name': 'Title',
        'src': 'a.readable_name'
      },
      {
        'name': 'Views',
        'src': 'a.view_score'
      },
      {
        'name': 'Participations',
        'src': 'a.participate_score'
      },
      {
        'name': 'Last Access',
        'src': 'a.last_access',
        'fmt': 'date'
      },
      {
        'name': 'First Access',
        'src': 'a.created_at',
        'fmt': 'date'
      },
      {
        'name': 'Action',
        'src': 'a.action_level'
      },
      {
        'name': 'Code',
        'src': 'a.asset_code'
      },
      {
        'name': 'Group Code',
        'src': 'a.asset_group_code'
      },
      {
        'name': 'Context Type',
        'src': 'a.context_type'
      },
      {
        'name': 'Context ID',
        'src': 'a.context_id'
      },
      {
        'name': 'Login ID',
        'src': 'u.login_id'
      },
      {
        'name': 'Section',
        'src': 'u.section_name',
      },
      {
        'name': 'Section ID',
        'src': 'u.section_id',
      },
      {
        'name': 'SIS Course ID',
        'src': 'u.sis_course_id',
        'sis': true
      },
      {
        'name': 'SIS Section ID',
        'src': 'u.sis_section_id',
        'sis': true
      },
      {
        'name': 'SIS Login ID',
        'src': 'u.sis_login_id',
        'sis': true
      },
      {
        'name': 'SIS User ID',
        'src': 'u.sis_user_id',
        'sis': true
      }
    ];
    var canSIS = false;
    for (var id in userData) {
      if (userData.hasOwnProperty(id)) {
        if (typeof userData[id].sis_user_id !== 'undefined' && 

userData[id].sis_user_id) {
          canSIS = true;
          break;
        }
      }
    }
    var CRLF = '\r\n';
    var hdr = [
    ];
    fields.map(function (e) {
      if (typeof e.sis === 'undefined' || (e.sis && canSIS)) {
        hdr.push(e.name);
      }
    });
    var t = hdr.join(',') + CRLF;
    var item,
    user,
    userId,
    fieldInfo,
    value;
    var fieldRegex = new RegExp('^([au])[.](.*)$');
    for (var i = 0; i < accessData.length; i++) {
      item = accessData[i].asset_user_access;
      userId = item.user_id;
      user = userData[userId];
      for (var j = 0; j < fields.length; j++) {
        if (typeof fields[j].sis !== 'undefined' && fields[j].sis && 

!canSIS) {
          continue;
        }
        fieldInfo = fields[j].src.split('.');
        value = fieldInfo[0] == 'a' ? item[fieldInfo[1]] : user

[fieldInfo[1]];
        if (value == null) {
          value = '';
        } 
        else {
          if (typeof fields[j].fmt !== 'undefined') {
            switch (fields[j].fmt) {
              case 'date':
                value = excelDate(value);
                break;
              default:
                break;
            }
          }
          if (typeof value === 'string') {
            var quote = false;
            if (value.indexOf('"') > - 1) {
              value = value.replace(/"/g, '""');
              quote = true;
            }
            if (value.indexOf(',') > - 1) {
              quote = true;
            }
            if (quote) {
              value = '"' + value + '"';
            }
          }
        }
        if (j > 0) {
          t += ',';
        }
        t += value;
      }
      t += CRLF;
    }
    return t;
  }
  function excelDate(timestamp) {
    try {
      if (!timestamp) {
        return '';
      }
      timestamp = timestamp.replace('Z', '.000Z');
      var dt = new Date(timestamp);
      if (typeof dt !== 'object') {
        return '';
      }
      var d = dt.getFullYear() + '-' +
      pad(1 + dt.getMonth()) + '-' +
      pad(dt.getDate()) + ' ' +
      pad(dt.getHours()) + ':' +
      pad(dt.getMinutes()) + ':' +
      pad(dt.getSeconds());
    } 
    catch (e) {
      errorHandler(e);
    }
    return d;
    function pad(n) {
      return n < 10 ? '0' + n : n;
    }
  }
  function errorHandler(e) {
    console.log(e);
    alert(e.message);
  }
}) ();
