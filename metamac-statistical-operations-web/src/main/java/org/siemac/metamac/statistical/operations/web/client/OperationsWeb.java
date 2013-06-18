package org.siemac.metamac.statistical.operations.web.client;

import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.siemac.metamac.sso.client.MetamacPrincipal;
import org.siemac.metamac.statistical.operations.core.constants.StatisticalOperationsConfigurationConstants;
import org.siemac.metamac.statistical.operations.core.constants.StatisticalOperationsConstants;
import org.siemac.metamac.statistical.operations.web.client.gin.OperationsWebGinjector;
import org.siemac.metamac.statistical.operations.web.client.utils.ConfigurationPropertiesUtils;
import org.siemac.metamac.web.common.client.MetamacEntryPoint;
import org.siemac.metamac.web.common.client.MetamacSecurityEntryPoint;
import org.siemac.metamac.web.common.client.events.LoginAuthenticatedEvent;
import org.siemac.metamac.web.common.client.gin.MetamacWebGinjector;
import org.siemac.metamac.web.common.client.utils.ApplicationEditionLanguages;
import org.siemac.metamac.web.common.client.utils.ApplicationOrganisation;
import org.siemac.metamac.web.common.client.widgets.MetamacNavBar;
import org.siemac.metamac.web.common.client.widgets.WaitingAsyncCallback;
import org.siemac.metamac.web.common.shared.GetLoginPageUrlAction;
import org.siemac.metamac.web.common.shared.GetLoginPageUrlResult;
import org.siemac.metamac.web.common.shared.GetNavigationBarUrlAction;
import org.siemac.metamac.web.common.shared.GetNavigationBarUrlResult;
import org.siemac.metamac.web.common.shared.LoadConfigurationPropertiesAction;
import org.siemac.metamac.web.common.shared.LoadConfigurationPropertiesResult;
import org.siemac.metamac.web.common.shared.MockCASUserAction;
import org.siemac.metamac.web.common.shared.MockCASUserResult;
import org.siemac.metamac.web.common.shared.ValidateTicketAction;
import org.siemac.metamac.web.common.shared.ValidateTicketResult;

import com.google.gwt.core.client.GWT;
import com.google.gwt.dom.client.Document;
import com.google.gwt.http.client.UrlBuilder;
import com.google.gwt.resources.client.ClientBundle;
import com.google.gwt.resources.client.CssResource;
import com.google.gwt.resources.client.CssResource.NotStrict;
import com.google.gwt.user.client.Window;
import com.gwtplatform.mvp.client.DelayedBindRegistry;

/**
 * Entry point classes define <code>onModuleLoad()</code>.
 */
public class OperationsWeb extends MetamacSecurityEntryPoint {

    private static final Boolean               SECURITY_ENABLED = true;

    private static Logger                      logger           = Logger.getLogger(OperationsWeb.class.getName());

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
        return new String[]{StatisticalOperationsConfigurationConstants.STATISTICAL_OPERATION_DEFAULT_UPDATE_FREQUENCY_CODELIST,
                StatisticalOperationsConfigurationConstants.INSTANCE_DEFAULT_FREQ_COLL, StatisticalOperationsConfigurationConstants.INSTANCE_DEFAULT_GEOGRAPHIC_GRANULARITY,
                StatisticalOperationsConfigurationConstants.INSTANCE_DEFAULT_TEMPORAL_GRANULARITY};
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
        ConfigurationPropertiesUtils.setOperationDefaultCodelistForUpdateFrequency(propertyValues
                .get(StatisticalOperationsConfigurationConstants.STATISTICAL_OPERATION_DEFAULT_UPDATE_FREQUENCY_CODELIST));
        ConfigurationPropertiesUtils.setInstanceDefaultCodelistForGeographicGranularity(propertyValues.get(StatisticalOperationsConfigurationConstants.INSTANCE_DEFAULT_GEOGRAPHIC_GRANULARITY));
        ConfigurationPropertiesUtils.setInstanceDefaultCodelistForTemporalGranularity(propertyValues.get(StatisticalOperationsConfigurationConstants.INSTANCE_DEFAULT_TEMPORAL_GRANULARITY));
        ConfigurationPropertiesUtils.setInstanceDefaultCodelistForFreqColl(propertyValues.get(StatisticalOperationsConfigurationConstants.INSTANCE_DEFAULT_FREQ_COLL));
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
        this.principal = principal;
    }
    
    @Override
    protected String getSecurityApplicationId() {
        return StatisticalOperationsConstants.SECURITY_APPLICATION_ID;
    }
    
    @Override
    protected MetamacWebGinjector getWebGinjector() {
        return ginjector;
    }
}
