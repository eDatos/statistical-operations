#!/bin/bash

HOME_PATH=metamac-statistical-operations
TRANSFER_PATH=$HOME_PATH/tmp
DEPLOY_TARGET_PATH_EXTERNAL=/servers/edatos-external/tomcats/edatos-external01/webapps
DEPLOY_TARGET_PATH_INTERNAL=/servers/edatos-internal/tomcats/edatos-internal01/webapps
ENVIRONMENT_RELATIVE_PATH_FILE=WEB-INF/classes/metamac/environment.xml
LOGBACK_RELATIVE_PATH_FILE=WEB-INF/classes/logback.xml
RESTART=1

if [ "$1" == "--no-restart" ]; then
    RESTART=0
fi


scp -r etc/deploy deploy@estadisticas.arte-consultores.com:$TRANSFER_PATH
scp metamac-statistical-operations-web/target/statistical-operations-internal-*.war deploy@estadisticas.arte-consultores.com:$TRANSFER_PATH/statistical-operations-internal.war
scp metamac-statistical-operations-external-web/target/statistical-operations-*.war deploy@estadisticas.arte-consultores.com:$TRANSFER_PATH/statistical-operations.war
ssh deploy@estadisticas.arte-consultores.com <<EOF

    chmod a+x $TRANSFER_PATH/deploy/*.sh;
    . $TRANSFER_PATH/deploy/utilities.sh
 
    ###
    # STATISTICAL-OPERATIONS-INTERNAL
    ###
    
    if [ $RESTART -eq 1 ]; then
        sudo service edatos-internal01 stop
        checkPROC "edatos-internal"
    fi

    # Update Process
    sudo rm -rf $DEPLOY_TARGET_PATH_INTERNAL/statistical-operations-internal
    sudo mv $TRANSFER_PATH/statistical-operations-internal.war $DEPLOY_TARGET_PATH_INTERNAL/statistical-operations-internal.war
    sudo unzip $DEPLOY_TARGET_PATH_INTERNAL/statistical-operations-internal.war -d $DEPLOY_TARGET_PATH_INTERNAL/statistical-operations-internal
    sudo rm -rf $DEPLOY_TARGET_PATH_INTERNAL/statistical-operations-internal.war

    # Restore Configuration
    sudo cp $HOME_PATH/environment_internal.xml $DEPLOY_TARGET_PATH_INTERNAL/statistical-operations-internal/$ENVIRONMENT_RELATIVE_PATH_FILE
    sudo cp $HOME_PATH/logback_internal.xml $DEPLOY_TARGET_PATH_INTERNAL/statistical-operations-internal/$LOGBACK_RELATIVE_PATH_FILE

    if [ $RESTART -eq 1 ]; then
        sudo chown -R edatos-internal.edatos-internal /servers/edatos-internal     
        sudo service edatos-internal01 start
    fi

    ###
    # STATISTICAL-OPERATIONS-EXTERNAL
    ###
    
    if [ $RESTART -eq 1 ]; then
        sudo service edatos-external01 stop
        checkPROC "edatos-external"
    fi

    # Update Process
    sudo rm -rf $DEPLOY_TARGET_PATH_EXTERNAL/statistical-operations
    sudo mv $TRANSFER_PATH/statistical-operations.war $DEPLOY_TARGET_PATH_EXTERNAL/statistical-operations.war
    sudo unzip $DEPLOY_TARGET_PATH_EXTERNAL/statistical-operations.war -d $DEPLOY_TARGET_PATH_EXTERNAL/statistical-operations
    sudo rm -rf $DEPLOY_TARGET_PATH_EXTERNAL/statistical-operations.war

    # Restore Configuration
    sudo cp $HOME_PATH/environment_external.xml $DEPLOY_TARGET_PATH_EXTERNAL/statistical-operations/$ENVIRONMENT_RELATIVE_PATH_FILE
    sudo cp $HOME_PATH/logback_external.xml $DEPLOY_TARGET_PATH_EXTERNAL/statistical-operations/$LOGBACK_RELATIVE_PATH_FILE

    if [ $RESTART -eq 1 ]; then
        sudo chown -R edatos-external.edatos-external /servers/edatos-external        
        sudo service edatos-external01 start
    fi

    echo "Finished deploy"

EOF
