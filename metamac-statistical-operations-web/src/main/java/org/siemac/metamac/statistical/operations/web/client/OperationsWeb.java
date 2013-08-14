package org.siemac.metamac.statistical.operations.web.client;

import java.util.Map;

import org.siemac.metamac.sso.client.MetamacPrincipal;
import org.siemac.metamac.statistical.operations.core.constants.StatisticalOperationsConfigurationConstants;
import org.siemac.metamac.statistical.operations.core.constants.StatisticalOperationsConstants;
import org.siemac.metamac.statistical.operations.web.client.gin.OperationsWebGinjector;
import org.siemac.metamac.statistical.operations.web.client.utils.ConfigurationPropertiesUtils;
import org.siemac.metamac.web.common.client.MetamacSecurityEntryPoint;
import org.siemac.metamac.web.common.client.gin.MetamacWebGinjector;

import com.google.gwt.core.client.GWT;

/**
 * Entry point classes define <code>onModuleLoad()</code>.
 */
public class OperationsWeb extends MetamacSecurityEntryPoint {

    private static final Boolean               SECURITY_ENABLED = true;

    private static MetamacPrincipal            principal;
    private static OperationsWebConstants      constants;
    private static OperationsWebCoreMessages   coreMessages;
    private static OperationsWebMessages       messages;

    public static final OperationsWebGinjector ginjector        = GWT.create(OperationsWebGinjector.class);

    @Override
    public void onModuleLoad() {
        setUncaughtExceptionHandler();

        prepareApplication(SECURITY_ENABLED);
    }

    @Override
    protected String[] getPropertiesToLoad() {
        return new String[]{StatisticalOperationsConfigurationConstants.DEFAULT_CODELIST_TEMPORAL_GRANULARITY_URN,
                StatisticalOperationsConfigurationConstants.DEFAULT_CODELIST_GEOGRAPHICAL_GRANULARITY_URN};
    }

    public static MetamacPrincipal getCurrentUser() {
        return OperationsWeb.principal;
    }

    public static OperationsWebConstants getConstants() {
        if (constants == null) {
            constants = (OperationsWebConstants) GWT.create(OperationsWebConstants.class);
        }
        return constants;
    }

    public static OperationsWebCoreMessages getCoreMessages() {
        if (coreMessages == null) {
            coreMessages = (OperationsWebCoreMessages) GWT.create(OperationsWebCoreMessages.class);
        }
        return coreMessages;
    }

    public static OperationsWebMessages getMessages() {
        if (messages == null) {
            messages = (OperationsWebMessages) GWT.create(OperationsWebMessages.class);
        }
        return messages;
    }

    public static OperationsWebGinjector getOperationsWebGinjector() {
        return ginjector;
    }

    public static void showErrorPage() {
        ginjector.getPlaceManager().revealErrorPlace(null);
    }

    @Override
    protected void setConfigurationProperties(Map<String, String> propertyValues) {
        super.setConfigurationProperties(propertyValues);
        ConfigurationPropertiesUtils.setDefaultCodelistTemporalGranularityUrn(propertyValues.get(StatisticalOperationsConfigurationConstants.DEFAULT_CODELIST_TEMPORAL_GRANULARITY_URN));
        ConfigurationPropertiesUtils.setDefaultCodelistGeographicalGranularityUrn(propertyValues.get(StatisticalOperationsConfigurationConstants.DEFAULT_CODELIST_GEOGRAPHICAL_GRANULARITY_URN));
    }

    @Override
    protected String getApplicationTitle() {
        return getConstants().appTitle();
    }

    @Override
    protected MetamacPrincipal getPrincipal() {
        return principal;
    }

    @Override
    protected void setPrincipal(MetamacPrincipal principal) {
        OperationsWeb.principal = principal;
    }

    @Override
    protected String getSecurityApplicationId() {
        return StatisticalOperationsConstants.SECURITY_APPLICATION_ID;
    }

    @Override
    protected MetamacWebGinjector getWebGinjector() {
        return ginjector;
    }

    @Override
    protected String getBundleName() {
        return "messages-statistical_operations-web";
    }
}
