package org.siemac.metamac.statistical.operations.web.server.utils;

import java.util.Locale;

import org.siemac.metamac.core.common.exception.utils.TranslateExceptions;
import org.siemac.metamac.core.common.lang.LocaleUtil;
import org.springframework.stereotype.Component;

@Component
public class WebTranslateExceptions extends TranslateExceptions {

    public String getTranslatedMessage(String code, String localString, String... parameters) {
        Locale locale = LocaleUtil.getLocaleFromLocaleString(localString);

        String translatedMessage = translateMessage(code, locale);

        if (parameters != null) {
            String[] translatedParameters = new String[parameters.length];
            for (int i = 0; i < parameters.length; i++) {
                translatedParameters[i] = getTranslatedParameter(parameters[i], locale);
            }
            translatedMessage = replaceParametersInMessage(translatedMessage, translatedParameters);
        }

        return translatedMessage;
    }

    protected String translateMessage(String code, Locale locale) {
        return LocaleUtil.getLocalizedMessageFromBundle("i18n/messages-statistical_operations-web", code, locale);
    }
}
