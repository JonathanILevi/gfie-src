(*
    Greenfish Icon Editor Pro
    Copyright (c) 2012-13 B. Szalkai

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

// Source of the locale identifiers: http://msdn.microsoft.com/en-us/goglobal/bb964664
unit Locales;

{$mode delphi}

interface

type
  TWindowsLocale = record
    Name: string;
    ID: word;
  end;

const
  WindowsLocales: array[0..237] of TWindowsLocale =
    ((Name: 'Afrikaans - South Africa'; ID: $0436),
    (Name: 'Albanian - Albania'; ID: $041c),
    (Name: 'Alsatian'; ID: $0484),
    (Name: 'Amharic - Ethiopia'; ID: $045e),
    (Name: 'Arabic - Saudi Arabia'; ID: $0401),
    (Name: 'Arabic - Algeria'; ID: $1401),
    (Name: 'Arabic - Bahrain'; ID: $3c01),
    (Name: 'Arabic - Egypt'; ID: $0c01),
    (Name: 'Arabic - Iraq'; ID: $0801),
    (Name: 'Arabic - Jordan'; ID: $2c01),
    (Name: 'Arabic - Kuwait'; ID: $3401),
    (Name: 'Arabic - Lebanon'; ID: $3001),
    (Name: 'Arabic - Libya'; ID: $1001),
    (Name: 'Arabic - Morocco'; ID: $1801),
    (Name: 'Arabic - Oman'; ID: $2001),
    (Name: 'Arabic - Qatar'; ID: $4001),
    (Name: 'Arabic - Syria'; ID: $2801),
    (Name: 'Arabic - Tunisia'; ID: $1c01),
    (Name: 'Arabic - U.A.E.'; ID: $3801),
    (Name: 'Arabic - Yemen'; ID: $2401),
    (Name: 'Armenian - Armenia'; ID: $042b),
    (Name: 'Assamese'; ID: $044d),
    (Name: 'Azeri (Cyrillic)'; ID: $082c),
    (Name: 'Azeri (Latin)'; ID: $042c),
    (Name: 'Bashkir'; ID: $046d),
    (Name: 'Basque'; ID: $042d),
    (Name: 'Belarusian'; ID: $0423),
    (Name: 'Bengali (India)'; ID: $0445),
    (Name: 'Bengali (Bangladesh)'; ID: $0845),
    (Name: 'Bosnian (Bosnia/Herzegovina)'; ID: $141A),
    (Name: 'Breton'; ID: $047e),
    (Name: 'Bulgarian'; ID: $0402),
    (Name: 'Burmese'; ID: $0455),
    (Name: 'Catalan'; ID: $0403),
    (Name: 'Cherokee - United States'; ID: $045c),
    (Name: 'Chinese - People''s Republic of China'; ID: $0804),
    (Name: 'Chinese - Singapore'; ID: $1004),
    (Name: 'Chinese - Taiwan'; ID: $0404),
    (Name: 'Chinese - Hong Kong SAR'; ID: $0c04),
    (Name: 'Chinese - Macao SAR'; ID: $1404),
    (Name: 'Corsican'; ID: $0483),
    (Name: 'Croatian'; ID: $041a),
    (Name: 'Croatian (Bosnia/Herzegovina)'; ID: $101a),
    (Name: 'Czech'; ID: $0405),
    (Name: 'Danish'; ID: $0406),
    (Name: 'Dari'; ID: $048c),
    (Name: 'Divehi'; ID: $0465),
    (Name: 'Dutch - Netherlands'; ID: $0413),
    (Name: 'Dutch - Belgium'; ID: $0813),
    (Name: 'Edo'; ID: $0466),
    (Name: 'English - United States'; ID: $0409),
    (Name: 'English - United Kingdom'; ID: $0809),
    (Name: 'English - Australia'; ID: $0c09),
    (Name: 'English - Belize'; ID: $2809),
    (Name: 'English - Canada'; ID: $1009),
    (Name: 'English - Caribbean'; ID: $2409),
    (Name: 'English - Hong Kong SAR'; ID: $3c09),
    (Name: 'English - India'; ID: $4009),
    (Name: 'English - Indonesia'; ID: $3809),
    (Name: 'English - Ireland'; ID: $1809),
    (Name: 'English - Jamaica'; ID: $2009),
    (Name: 'English - Malaysia'; ID: $4409),
    (Name: 'English - New Zealand'; ID: $1409),
    (Name: 'English - Philippines'; ID: $3409),
    (Name: 'English - Singapore'; ID: $4809),
    (Name: 'English - South Africa'; ID: $1c09),
    (Name: 'English - Trinidad'; ID: $2c09),
    (Name: 'English - Zimbabwe'; ID: $3009),
    (Name: 'Estonian'; ID: $0425),
    (Name: 'Faroese'; ID: $0438),
    (Name: 'Farsi'; ID: $0429),
    (Name: 'Filipino'; ID: $0464),
    (Name: 'Finnish'; ID: $040b),
    (Name: 'French - France'; ID: $040c),
    (Name: 'French - Belgium'; ID: $080c),
    (Name: 'French - Cameroon'; ID: $2c0c),
    (Name: 'French - Canada'; ID: $0c0c),
    (Name: 'French - Democratic Rep. of Congo'; ID: $240c),
    (Name: 'French - Cote d''Ivoire'; ID: $300c),
    (Name: 'French - Haiti'; ID: $3c0c),
    (Name: 'French - Luxembourg'; ID: $140c),
    (Name: 'French - Mali'; ID: $340c),
    (Name: 'French - Monaco'; ID: $180c),
    (Name: 'French - Morocco'; ID: $380c),
    (Name: 'French - North Africa'; ID: $e40c),
    (Name: 'French - Reunion'; ID: $200c),
    (Name: 'French - Senegal'; ID: $280c),
    (Name: 'French - Switzerland'; ID: $100c),
    (Name: 'French - West Indies'; ID: $1c0c),
    (Name: 'Frisian - Netherlands'; ID: $0462),
    (Name: 'Fulfulde - Nigeria'; ID: $0467),
    (Name: 'FYRO Macedonian'; ID: $042f),
    (Name: 'Galician'; ID: $0456),
    (Name: 'Georgian'; ID: $0437),
    (Name: 'German - Germany'; ID: $0407),
    (Name: 'German - Austria'; ID: $0c07),
    (Name: 'German - Liechtenstein'; ID: $1407),
    (Name: 'German - Luxembourg'; ID: $1007),
    (Name: 'German - Switzerland'; ID: $0807),
    (Name: 'Greek'; ID: $0408),
    (Name: 'Greenlandic'; ID: $046f),
    (Name: 'Guarani - Paraguay'; ID: $0474),
    (Name: 'Gujarati'; ID: $0447),
    (Name: 'Hausa - Nigeria'; ID: $0468),
    (Name: 'Hawaiian - United States'; ID: $0475),
    (Name: 'Hebrew'; ID: $040d),
    (Name: 'Hindi'; ID: $0439),
    (Name: 'Hungarian'; ID: $040e),
    (Name: 'Ibibio - Nigeria'; ID: $0469),
    (Name: 'Icelandic'; ID: $040f),
    (Name: 'Igbo - Nigeria'; ID: $0470),
    (Name: 'Indonesian'; ID: $0421),
    (Name: 'Inuktitut'; ID: $045d),
    (Name: 'Irish'; ID: $083c),
    (Name: 'Italian - Italy'; ID: $0410),
    (Name: 'Italian - Switzerland'; ID: $0810),
    (Name: 'Japanese'; ID: $0411),
    (Name: 'K''iche'; ID: $0486),
    (Name: 'Kannada'; ID: $044b),
    (Name: 'Kanuri - Nigeria'; ID: $0471),
    (Name: 'Kashmiri'; ID: $0860),
    (Name: 'Kashmiri (Arabic)'; ID: $0460),
    (Name: 'Kazakh'; ID: $043f),
    (Name: 'Khmer'; ID: $0453),
    (Name: 'Kinyarwanda'; ID: $0487),
    (Name: 'Konkani'; ID: $0457),
    (Name: 'Korean'; ID: $0412),
    (Name: 'Kyrgyz (Cyrillic)'; ID: $0440),
    (Name: 'Lao'; ID: $0454),
    (Name: 'Latin'; ID: $0476),
    (Name: 'Latvian'; ID: $0426),
    (Name: 'Lithuanian'; ID: $0427),
    (Name: 'Luxembourgish'; ID: $046e),
    (Name: 'Malay - Malaysia'; ID: $043e),
    (Name: 'Malay - Brunei Darussalam'; ID: $083e),
    (Name: 'Malayalam'; ID: $044c),
    (Name: 'Maltese'; ID: $043a),
    (Name: 'Manipuri'; ID: $0458),
    (Name: 'Maori - New Zealand'; ID: $0481),
    (Name: 'Mapudungun'; ID: $0471),
    (Name: 'Marathi'; ID: $044e),
    (Name: 'Mohawk'; ID: $047c),
    (Name: 'Mongolian (Cyrillic)'; ID: $0450),
    (Name: 'Mongolian (Mongolian)'; ID: $0850),
    (Name: 'Nepali'; ID: $0461),
    (Name: 'Nepali - India'; ID: $0861),
    (Name: 'Norwegian (Bokmal)'; ID: $0414),
    (Name: 'Norwegian (Nynorsk)'; ID: $0814),
    (Name: 'Occitan'; ID: $0482),
    (Name: 'Oriya'; ID: $0448),
    (Name: 'Oromo'; ID: $0472),
    (Name: 'Papiamentu'; ID: $0479),
    (Name: 'Pashto'; ID: $0463),
    (Name: 'Polish'; ID: $0415),
    (Name: 'Portuguese - Brazil'; ID: $0416),
    (Name: 'Portuguese - Portugal'; ID: $0816),
    (Name: 'Punjabi'; ID: $0446),
    (Name: 'Punjabi (Pakistan)'; ID: $0846),
    (Name: 'Quecha - Bolivia'; ID: $046B),
    (Name: 'Quecha - Ecuador'; ID: $086B),
    (Name: 'Quecha - Peru'; ID: $0C6B),
    (Name: 'Rhaeto-Romanic'; ID: $0417),
    (Name: 'Romanian'; ID: $0418),
    (Name: 'Romanian - Moldava'; ID: $0818),
    (Name: 'Russian'; ID: $0419),
    (Name: 'Russian - Moldava'; ID: $0819),
    (Name: 'Sami (Lappish)'; ID: $043b),
    (Name: 'Sanskrit'; ID: $044f),
    (Name: 'Scottish Gaelic'; ID: $043c),
    (Name: 'Sepedi'; ID: $046c),
    (Name: 'Serbian (Cyrillic)'; ID: $0c1a),
    (Name: 'Serbian (Latin)'; ID: $081a),
    (Name: 'Sindhi - India'; ID: $0459),
    (Name: 'Sindhi - Pakistan'; ID: $0859),
    (Name: 'Sinhalese - Sri Lanka'; ID: $045b),
    (Name: 'Slovak'; ID: $041b),
    (Name: 'Slovenian'; ID: $0424),
    (Name: 'Somali'; ID: $0477),
    (Name: 'Sorbian'; ID: $042e),
    (Name: 'Spanish - Spain (Modern Sort)'; ID: $0c0a),
    (Name: 'Spanish - Spain (Traditional Sort)'; ID: $040a),
    (Name: 'Spanish - Argentina'; ID: $2c0a),
    (Name: 'Spanish - Bolivia'; ID: $400a),
    (Name: 'Spanish - Chile'; ID: $340a),
    (Name: 'Spanish - Colombia'; ID: $240a),
    (Name: 'Spanish - Costa Rica'; ID: $140a),
    (Name: 'Spanish - Dominican Republic'; ID: $1c0a),
    (Name: 'Spanish - Ecuador'; ID: $300a),
    (Name: 'Spanish - El Salvador'; ID: $440a),
    (Name: 'Spanish - Guatemala'; ID: $100a),
    (Name: 'Spanish - Honduras'; ID: $480a),
    (Name: 'Spanish - Latin America'; ID: $580a),
    (Name: 'Spanish - Mexico'; ID: $080a),
    (Name: 'Spanish - Nicaragua'; ID: $4c0a),
    (Name: 'Spanish - Panama'; ID: $180a),
    (Name: 'Spanish - Paraguay'; ID: $3c0a),
    (Name: 'Spanish - Peru'; ID: $280a),
    (Name: 'Spanish - Puerto Rico'; ID: $500a),
    (Name: 'Spanish - United States'; ID: $540a),
    (Name: 'Spanish - Uruguay'; ID: $380a),
    (Name: 'Spanish - Venezuela'; ID: $200a),
    (Name: 'Sutu'; ID: $0430),
    (Name: 'Swahili'; ID: $0441),
    (Name: 'Swedish'; ID: $041d),
    (Name: 'Swedish - Finland'; ID: $081d),
    (Name: 'Syriac'; ID: $045a),
    (Name: 'Tajik'; ID: $0428),
    (Name: 'Tamazight (Arabic)'; ID: $045f),
    (Name: 'Tamazight (Latin)'; ID: $085f),
    (Name: 'Tamil'; ID: $0449),
    (Name: 'Tatar'; ID: $0444),
    (Name: 'Telugu'; ID: $044a),
    (Name: 'Thai'; ID: $041e),
    (Name: 'Tibetan - Bhutan'; ID: $0851),
    (Name: 'Tibetan - People''s Republic of China'; ID: $0451),
    (Name: 'Tigrigna - Eritrea'; ID: $0873),
    (Name: 'Tigrigna - Ethiopia'; ID: $0473),
    (Name: 'Tsonga'; ID: $0431),
    (Name: 'Tswana'; ID: $0432),
    (Name: 'Turkish'; ID: $041f),
    (Name: 'Turkmen'; ID: $0442),
    (Name: 'Uighur - China'; ID: $0480),
    (Name: 'Ukrainian'; ID: $0422),
    (Name: 'Urdu'; ID: $0420),
    (Name: 'Urdu - India'; ID: $0820),
    (Name: 'Uzbek (Cyrillic)'; ID: $0843),
    (Name: 'Uzbek (Latin)'; ID: $0443),
    (Name: 'Venda'; ID: $0433),
    (Name: 'VietNamese'; ID: $042a),
    (Name: 'Welsh'; ID: $0452),
    (Name: 'Wolof'; ID: $0488),
    (Name: 'Xhosa'; ID: $0434),
    (Name: 'Yakut'; ID: $0485),
    (Name: 'Yi'; ID: $0478),
    (Name: 'Yiddish'; ID: $043d),
    (Name: 'Yoruba'; ID: $046a),
    (Name: 'Zulu'; ID: $0435),
    (Name: 'HID (Human Interface Device)'; ID: $04ff));

function LangToStr(localeId: integer): string;

implementation

uses SysUtils, LangPack;

function LangToStr(localeId: integer): string;
var
  i: integer;
begin
  if localeId = 0 then
    Exit(lpGet('RP_LANG_NEUTRAL'));
  for i := 0 to Length(WindowsLocales) - 1 do
  with WindowsLocales[i] do
  begin
    if ID = localeId then
      Exit(Name);
  end;
  Exit('#' + IntToStr(localeId));
end;

end.
